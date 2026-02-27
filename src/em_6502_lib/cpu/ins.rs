use crate::{prelude::*, cpu::CPU6502};
use CPUAddrMode::*;

/// CPU instruction implementation functions
impl CPU6502 {
    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the ADC Instruction.
    pub fn adc(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("ADC".to_string());

        let init_val = self.ac;
        let add_val = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for ADC"),
        }.wrapping_add(if self.ps.test_bit(BitMasks::C) {1} else {0});

        self.ac = self.ac.wrapping_add(add_val);

        self.update_v_flag(init_val, add_val, self.ac);
        self.ps.set_bit(BitMasks::C, self.ps.test_bit(BitMasks::V));
        self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);
        self.ps.set_bit(BitMasks::Z, self.ac == 0);

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the AND Instruction
    pub fn and(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("AND".to_string());

        self.ac &= match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for AND"),
        };

        self.update_z(self.ac);
        self.update_n_flag(self.ac);

        self.restore_debug_msg();
    }

    /// 1 - 6 cycles
    /// 
    /// Implements functionality of the ASL Instruction
    pub fn asl(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("ASL".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => {
                let ret = Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord);
                self.debug_imm("get zpx addr".to_string());
                self.cycles += 1;
                ret
            }
            ABS => Some(self.fetch_next_word()),
            ABX => {
                let ret = Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord));
                self.debug_imm("get abx addr".to_string());
                self.cycles += 1;
                ret
            }
            _ => panic!("Invalid address mode for ASL"),
        };

        let byte = match addr {
            None => {
                self.debug_imm("shift ac left".to_string());
                &mut self.ac
            }
            Some(a) => {
                self.debug_imm(format!("shift byte at {a:#06X} left (3 cycles)"));
                let ret = self.cpu_mem.mut_byte_at(a);
                self.cycles += 2;
                ret
            }
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shl(1);
        self.cycles += 1;
        let new_byte = byte.clone();
        
        self.ps.set_bit(BitMasks::C, orig_byte & 0b1000_0000 != 0);
        self.update_z(self.ac);
        self.update_n_flag(new_byte);
        
        self.restore_debug_msg();
    }

    /// 1 - 3 cycles
    /// 
    /// Implements functionality for listed Instructions:
    /// - BCC
    /// - BCS
    /// - BEQ
    /// - BMI
    /// - BNE
    /// - BPL
    /// - BRK
    /// - BVC
    /// - BVS
    pub fn branch(&mut self, cond: bool) {
        // Debug message is set and restored from caller
        let operand = self.fetch_next_byte();
        if cond {
            self.push_debug_msg("branch taken".to_string());
            self.fetch_next_byte();
            let orig = self.pc;
            self.pc = self.pc.wrapping_add_signed((operand as i8 - 1).into());
            self.cycles += 1;

            if self.pc / 256 != orig / 256 {
                self.debug_imm("page_crossed".to_string());
                self.cycles += 1;
            }

            self.restore_debug_msg();
        } else {
            self.debug_imm("branch not taken".to_string());
            // self.pc = self.pc.wrapping_sub(1);
        }

    }

    /// 6 cycles
    /// 
    /// Implements functionality of the BRK Instruction.
    pub fn brk(&mut self) {
        self.push_debug_msg("BRK".to_string());
        self.fetch_next_byte();

        self.push_debug_msg("push_pc".to_string());
        self.ps.set_bit(BitMasks::B, true);
        self.push_word(self.pc);
        self.restore_debug_msg();

        self.push_debug_msg("push_ps".to_string());
        self.push_byte(self.ps.to_inner());
        self.restore_debug_msg();

        self.push_debug_msg("jmp_to_loc_at_irq_vector".to_string());
        self.pc = self.fetch_word_at(0xFFFE);
        self.restore_debug_msg();

        self.restore_debug_msg();
    }

    /// 2 - 3 cycles
    /// 
    /// Implements functionality of the BIT Instruction
    pub fn bit(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("BIT".to_string());

        let val = match mode {
            ZPG => self.zpg(),
            ABS => self.abs(),
            _ => panic!("Invalid address mode for BIT")
        };

        self.update_z(self.ac & val);
        self.update_n_flag(val);
        self.ps.set_bit(BitMasks::V, val & 0b0100_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    pub fn cmp(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("CMP".to_string());

        let val = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for CMP"),
        };

        self.ps.set_bit(BitMasks::C, self.ac >= val);
        self.ps.set_bit(BitMasks::Z, self.ac == val);
        self.ps.set_bit(BitMasks::N, val & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 3 cycles
    pub fn cpx(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("CPX".to_string());

        let val = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ABS => self.abs(),
            _ => panic!("Invalid addressing mode for CPX"),
        };

        self.ps.set_bit(BitMasks::C, self.rx >= val);
        self.ps.set_bit(BitMasks::Z, self.rx == val);
        self.ps.set_bit(BitMasks::N, val & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 3 cycles
    pub fn cpy(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("CPY".to_string());

        let val = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ABS => self.abs(),
            _ => panic!("Invalid addressing mode for CPX"),
        };

        self.ps.set_bit(BitMasks::C, self.ry >= val);
        self.ps.set_bit(BitMasks::Z, self.ry == val);
        self.ps.set_bit(BitMasks::N, val & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 4 - 6 cycles
    pub fn dec(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("DEC".to_string());

        let addr = match mode {
            ZPG => {
                self.push_debug_msg("zpg_addr".to_string());
                let ret = self.fetch_next_byte() as CPUWord;
                self.debug_ret_word("zpg_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ZPX => {
                self.push_debug_msg("zpx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                self.debug_ret_word("zpx_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABS => {
                self.push_debug_msg("abs_addr".to_string());
                let ret = self.fetch_next_word();
                self.debug_ret_word("abs_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABX => {
                self.push_debug_msg("abx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_word().wrapping_add(self.rx as CPUWord);
                self.cycles += 1;
                self.debug_ret_word("abx_addr", ret);
                self.restore_debug_msg();
                ret
            }
            _ => panic!("Invalid addressing mode for INC"),
        };

        let mut byte = self.cpu_mem.byte_at(addr as CPUWord);
        self.debug_imm(format!("fetch byte at {addr:#06X} ({byte:#04X})"));
        self.cycles += 1;
        
        byte = byte.wrapping_sub(1);
        self.debug_imm(format!("decrement byte at {addr:#06X} -> ({byte:#04X})"));
        *self.cpu_mem.mut_byte_at(addr as CPUWord) = byte;
        self.cycles += 1;
        
        self.ps.set_bit(BitMasks::Z, byte == 0);
        self.ps.set_bit(BitMasks::N, byte & 0b1000_0000 != 0);
        
        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    pub fn eor(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("EOR".to_string());

        let val = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid addressing mode for EOR"),
        };

        self.ac ^= val;
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    pub fn inc(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("INC".to_string());

        let addr = match mode {
            ZPG => {
                self.push_debug_msg("zpg_addr".to_string());
                let ret = self.fetch_next_byte() as CPUWord;
                self.debug_ret_word("zpg_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ZPX => {
                self.push_debug_msg("zpx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                self.debug_ret_word("zpx_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABS => {
                self.push_debug_msg("abs_addr".to_string());
                let ret = self.fetch_next_word();
                self.debug_ret_word("abs_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABX => {
                self.push_debug_msg("abx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_word().wrapping_add(self.rx as CPUWord);
                self.cycles += 1;
                self.debug_ret_word("abx_addr", ret);
                self.restore_debug_msg();
                ret
            }
            _ => panic!("Invalid addressing mode for INC"),
        };

        let mut byte = self.cpu_mem.byte_at(addr as CPUWord);
        self.debug_imm(format!("fetch byte at {addr:#06X} ({byte:#04X})"));
        self.cycles += 1;

        byte = byte.wrapping_add(1);
        self.debug_imm(format!("increment byte at {addr:#06X} -> ({byte:#04X})"));
        *self.cpu_mem.mut_byte_at(addr as CPUWord) = byte;
        self.cycles += 1;

        self.ps.set_bit(BitMasks::Z, byte == 0);
        self.ps.set_bit(BitMasks::N, byte & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 2 or 4 cycles
    /// 
    /// Currently does not implement original 6502 behaviour.
    /// 
    /// When performing an indirect jump whre the indirect vector fell upon a page boundary,
    /// the original 6502 would behave unexpectedly.
    /// 
    /// See http://www.6502.org/users/obelisk/6502/reference.html#JMP for more information.
    /// 
    /// Moot point if indirect vectors are ensured not to fall upon page boundaries
    pub fn jmp(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("JMP".to_string());

        self.pc = match mode {
            ABS => self.fetch_next_word(),
            IND => {
                let addr = self.fetch_next_word();
                self.fetch_word_at(addr)
            },
            _ => panic!("Invalid addressing mode for JMP"),
        };

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the LDA Instruction
    pub fn lda(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("LDA".to_string());

        self.ac = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for LDA"),
        };

        self.update_z(self.ac);
        self.update_n_flag(self.ac);
        
        self.restore_debug_msg();
    }

    /// 1 - 3 cycles
    pub fn ldx(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("LDX".to_string());

        self.rx = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPY => self.zpy(),
            ABS => self.abs(),
            ABY => self.aby(),
            _ => panic!("Invalid address mode for LDX")
        };

        self.ps.set_bit(BitMasks::Z, self.rx == 0);
        self.ps.set_bit(BitMasks::N, self.rx & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 3 cycles
    pub fn ldy(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("LDY".to_string());

        self.ry = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            _ => panic!("Invalid address mode for LDX")
        };

        self.ps.set_bit(BitMasks::Z, self.ry == 0);
        self.ps.set_bit(BitMasks::N, self.ry & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 6 cycles
    pub fn lsr(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("LSR".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => {
                let ret = Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord);
                self.debug_imm("get zpx addr".to_string());
                self.cycles += 1;
                ret
            }
            ZPX => Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord),
            ABS => Some(self.fetch_next_word()),
            ABX => {
                let ret = Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord));
                self.debug_imm("get abx addr".to_string());
                self.cycles += 1;
                ret
            }
            _ => panic!("Invalid addressing mode for LSR"),
        };
        
        let byte = match addr { 
            None => {
                self.debug_imm("shift ac right".to_string());
                &mut self.ac
            }
            Some(a) => {
                self.debug_imm(format!("shift byte at {a:#06X} right"));
                let ret = self.cpu_mem.mut_byte_at(a);
                self.cycles += 2;
                ret
            }
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shr(1);
        self.cycles += 1;
        let new_byte = byte.clone();
        
        self.ps.set_bit(BitMasks::C, orig_byte & 0b0000_0001 != 0);
        self.ps.set_bit(BitMasks::Z, new_byte == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);
        
        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    pub fn ora(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("ORA".to_string());

        self.ac |= match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for ORA"),
        };

        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);
        
        self.restore_debug_msg();
    }

    /// 1 - 6 cycles
    pub fn rol(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("ROL".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => {
                let ret = Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord);
                self.debug_imm("get zpx addr".to_string());
                self.cycles += 1;
                ret
            }
            ABS => Some(self.fetch_next_word()),
            ABX => {
                let ret = Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord));
                self.debug_imm("get abx addr".to_string());
                self.cycles += 1;
                ret
            }
            _ => panic!("Invalid address mode for ROL"),
        };

        let byte = match addr {
            None => {
                self.debug_imm("rotate ac left".to_string());
                &mut self.ac
            }
            Some(a) => {
                self.debug_imm(format!("rotate byte at {a:#06X} left"));
                let ret = self.cpu_mem.mut_byte_at(a);
                self.cycles += 2;
                ret
            }
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shl(1).wrapping_add(if self.ps.test_bit(BitMasks::C) {0b0000_0001} else {0});
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::C, orig_byte & 0b1000_0000 != 0);
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 6 cycles
    pub fn ror(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("ROR".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => {
                let ret = Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord);
                self.debug_imm("get zpx addr".to_string());
                self.cycles += 1;
                ret
            }
            ABS => Some(self.fetch_next_word()),
            ABX => {
                let ret = Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord));
                self.debug_imm("get abx addr".to_string());
                self.cycles += 1;
                ret
            }
            _ => panic!("Invalid address mode for ROR"),
        };

        let byte = match addr {
            None => {
                self.debug_imm("rotate ac right".to_string());
                &mut self.ac
            }
            Some(a) => {
                self.debug_imm(format!("rotate byte at {a:#06X} right"));
                let ret = self.cpu_mem.mut_byte_at(a);
                self.cycles += 2;
                ret
            }
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shr(1).wrapping_add(if self.ps.test_bit(BitMasks::C) {0b1000_0000} else {0});
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::C, orig_byte & 0b0000_0001 != 0);
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    pub fn sbc(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("SBC".to_string());

        let val = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for SBC"),
        };

        let orig_val = self.ac;
        self.ac = self.ac.wrapping_sub(val.wrapping_add(if !self.ps.test_bit(BitMasks::C) {1} else {0}));

        self.update_v_flag(orig_val, val, self.ac);
        if self.ps.test_bit(BitMasks::V) {
            self.ps.set_bit(BitMasks::C, false);
        }
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);

        self.restore_debug_msg();
    }

    /// 2 - 5 cycles
    pub fn sta(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("STA".to_string());

        let addr = match mode {
            ZPG => {
                self.push_debug_msg("zpg_addr".to_string());
                let ret = self.fetch_next_byte() as CPUWord;
                self.debug_ret_word("zpg_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ZPX => {
                self.push_debug_msg("zpx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                self.debug_ret_word("zpx_addr", ret);
                self.restore_debug_msg();
                ret
            },
            ABS => {
                self.push_debug_msg("abs_addr".to_string());
                let ret = self.fetch_next_word();
                self.debug_ret_word("abs_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABX => {
                self.push_debug_msg("abx_addr".to_string());
                let ret = self.fetch_next_word().wrapping_add(self.rx as CPUWord);
                self.cycles += 1;
                self.debug_ret_word("abx_addr", ret);
                self.restore_debug_msg();
                ret
            },
            ABY => {
                self.push_debug_msg("aby_addr".to_string());
                let ret = self.fetch_next_word().wrapping_add(self.ry as CPUWord);
                self.cycles += 1;
                self.debug_ret_word("aby_addr", ret);
                self.restore_debug_msg();
                ret
            },
            IDX => {
                self.push_debug_msg("idx_addr".to_string());
                let addr = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                let ret = self.fetch_word_at(addr);
                self.debug_ret_word("idx_addr", ret);
                self.restore_debug_msg();
                ret
            },
            IDY => {
                self.push_debug_msg("idy_addr".to_string());
                let addr = self.fetch_next_byte();
                let ret = self.fetch_word_at(addr as CPUWord).wrapping_add(self.ry as CPUWord);
                self.cycles += 1;
                self.debug_ret_word("idy_addr", ret);
                self.restore_debug_msg();
                ret
            },
            _ => panic!("Invalid address mode for STA"),
        };

        self.debug_imm(format!("store ac ({:#04X}) at {addr:#06X}", self.ac));
        *self.cpu_mem.mut_byte_at(addr) = self.ac;
        self.cycles += 1;

        self.restore_debug_msg();
    }

    /// 2 - 3 cycles
    pub fn stx(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("STX".to_string());

        let addr = match mode {
            ZPG => {
                self.push_debug_msg("zpg_addr".to_string());
                let ret = self.fetch_next_byte() as CPUWord;
                self.debug_ret_word("zpg_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ZPY => {
                self.push_debug_msg("zpy_addr".to_string());
                self.debug();
                let addr = self.fetch_next_byte().wrapping_add(self.ry) as CPUWord;
                self.cycles += 1;
                self.debug_ret_word("zpy_addr", addr);
                self.restore_debug_msg();
                addr
            },
            ABS => {
                self.push_debug_msg("abs_addr".to_string());
                let ret = self.fetch_next_word();
                self.debug_ret_word("abs_addr", ret);
                self.restore_debug_msg();
                ret
            }
            _ => panic!("Invalid address mode for STX"),
        };

        self.debug_imm(format!("store rx ({:#04X}) at {addr:#06X}", self.rx));
        *self.cpu_mem.mut_byte_at(addr) = self.ac;
        self.cycles += 1;

        self.restore_debug_msg();
    }

    /// 2 - 3 cycles
    pub fn sty(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("STY".to_string());

        let addr = match mode {
            ZPG => {
                self.push_debug_msg("zpg_addr".to_string());
                let ret = self.fetch_next_byte() as CPUWord;
                self.debug_ret_word("zpg_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ZPX => {
                self.push_debug_msg("zpx_addr".to_string());
                self.debug();
                let addr = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                self.debug_ret_word("zpg_addr", addr);
                self.restore_debug_msg();
                addr
            },
            ABS => {
                self.push_debug_msg("abs_addr".to_string());
                let ret = self.fetch_next_word();
                self.debug_ret_word("abs_addr", ret);
                self.restore_debug_msg();
                ret
            }
            _ => panic!("Invalid address mode for STY"),
        };

        self.debug_imm(format!("store ry ({:#04X}) at {addr:#06X}", self.ry));
        *self.cpu_mem.mut_byte_at(addr) = self.ac;
        self.cycles += 1;

        self.restore_debug_msg();
    }

    pub fn stz(&mut self, mode: CPUAddrMode) {
        self.push_debug_msg("STZ".to_string());

        let addr = match mode {
            ZPG => {
                self.push_debug_msg("zpg_addr".to_string());
                let ret = self.fetch_next_byte() as CPUWord;
                self.debug_ret_word("zpg_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ZPX => {
                self.push_debug_msg("zpx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                self.debug_ret_word("zpx_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABS => {
                self.push_debug_msg("abs_addr".to_string());
                let ret = self.fetch_next_word();
                self.debug_ret_word("abs_addr", ret);
                self.restore_debug_msg();
                ret
            }
            ABX => {
                self.push_debug_msg("abx_addr".to_string());
                self.debug();
                let ret = self.fetch_next_word().wrapping_add(self.rx as CPUWord);
                self.cycles += 1;
                self.debug_ret_word("abx_addr", ret);
                self.restore_debug_msg();
                ret
            }
            _ => panic!("Invalid address mode for STZ"),
        };

        self.debug_imm(format!("set byte at {addr:#06X} to 0"));
        *self.cpu_mem.mut_byte_at(addr) = 0;
        self.cycles += 1;

        self.restore_debug_msg();
    }
}