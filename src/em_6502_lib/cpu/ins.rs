use crate::{prelude::*, cpu::CPU6502};

/// CPU instruction implementation functions
impl CPU6502 {
    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the ADC Instruction.
    pub fn adc(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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
        
        self.debug();
        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the AND Instruction
    pub fn and(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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

        self.debug();
        self.restore_debug_msg();
    }

    /// 1 - 6 cycles
    /// 
    /// Implements functionality of the ASL Instruction
    pub fn asl(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("ASL".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord),
            ABS => Some(self.fetch_next_word()),
            ABX => Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord)),
            _ => panic!("Invalid address mode for ASL"),
        };

        let byte = match addr {
            None => &mut self.ac,
            Some(a) => self.cpu_mem.mut_byte_at(a),
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shl(1);
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::C, orig_byte & 0b1000_0000 != 0);
        self.update_z(self.ac);
        self.update_n_flag(new_byte);

        self.debug();
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
        // Debug message is set and restored from calling executiong function
        self.debug();
        self.cycles += 1;
        if cond {
            self.debug_imm("branch_success".to_string());

            let orig = self.pc;
            self.pc = self.pc.wrapping_add_signed((self.fetch_next_byte() as i8).into());

            if self.pc / 256 != orig / 256 {
                self.cycles += 1;
                self.debug_imm("page_crossed".to_string());
            }
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
        self.debug();
        self.restore_debug_msg();

        self.restore_debug_msg();
    }

    /// 2 - 3 cycles
    /// 
    /// Implements functionality of the BIT Instruction
    pub fn bit(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("BIT".to_string());

        let val = match mode {
            ZPG => self.zpg(),
            ABS => self.abs(),
            _ => panic!("Invalid address mode for BIT")
        };

        self.cycles += 1;

        self.update_z(self.ac & val);
        self.update_n_flag(val);
        self.ps.set_bit(BitMasks::V, val & 0b0100_0000 != 0);

        self.debug();
        self.restore_debug_msg();
    }

    pub fn cmp(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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

        self.debug();
        self.restore_debug_msg();
    }

    pub fn cpx(&mut self, mode: CPUAddrMode) {
        use  CPUAddrMode::*;
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

        self.debug();
        self.restore_debug_msg();
    }

    pub fn cpy(&mut self, mode: CPUAddrMode) {
        use  CPUAddrMode::*;
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

        self.debug();
        self.restore_debug_msg();
    }

    pub fn dec(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("DEC".to_string());

        let addr = match mode {
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            _ => panic!("Invalid addressing mode for DEC"),
        };

        let byte = self.cpu_mem.mut_byte_at(addr as CPUWord);
        self.cycles += 1;
        let orig_byte = byte.clone();
        
        *byte = byte.wrapping_sub(1);
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::Z, new_byte == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);

        self.debug_imm(format!("byte at {addr:#06X} decremented ({orig_byte} -> {new_byte})"));
        self.restore_debug_msg();
    }

    pub fn eor(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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

        self.debug();
        self.restore_debug_msg();
    }

    pub fn inc(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("INC".to_string());

        let addr = match mode {
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            _ => panic!("Invalid addressing mode for INC"),
        };

        let byte = self.cpu_mem.mut_byte_at(addr as CPUWord);
        self.cycles += 1;
        let orig_byte = byte.clone();

        *byte = byte.wrapping_add(1);
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::Z, new_byte == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);

        self.debug_imm(format!("byte at {addr:#06X} incremented ({orig_byte} -> {new_byte})"));
        self.debug();
        self.restore_debug_msg();
    }

    /// Currently does not implement original 6502 behaviour.
    /// 
    /// When performing an indirect jump whre the indirect vector fell upon a page boundary,
    /// the original 6502 would behave unexpectedly.
    /// 
    /// See http://www.6502.org/users/obelisk/6502/reference.html#JMP for more information.
    /// 
    /// Moot point if indirect vectors are ensured not to fall upon page boundaries
    pub fn jmp(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("JMP".to_string());

        self.pc = match mode {
            ABS => self.fetch_next_word(),
            IND => {
                let addr = self.fetch_next_word();
                self.fetch_word_at(addr)
            },
            _ => panic!("Invalid addressing mode for JMP"),
        };
        self.debug_imm("set pc".to_string());

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the LDA Instruction
    pub fn lda(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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

        self.debug_imm("ret -> ac".to_string());
        
        self.restore_debug_msg();
    }

    pub fn ldx(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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

        self.debug_imm("ret -> rx".to_string());

        self.restore_debug_msg();
    }

    pub fn ldy(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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

        self.debug_imm("ret -> ry".to_string());

        self.restore_debug_msg();
    }

    pub fn lsr(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("LSR".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord),
            ABS => Some(self.fetch_next_word()),
            ABX => Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord)),
            _ => panic!("Invalid addressing mode for LSR"),
        };

        let byte = match addr { 
            None => &mut self.ac,
            Some(addr) => self.cpu_mem.mut_byte_at(addr),
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shr(1);
        self.cycles += 1;
        let new_byte = byte.clone();

        
        self.ps.set_bit(BitMasks::C, orig_byte & 0b0000_0001 != 0);
        self.ps.set_bit(BitMasks::Z, new_byte == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);
        
        self.debug_imm(format!("{} arithmetically shifted right ({orig_byte} -> {new_byte})", if let Some(a) = addr {format!("byte at {:#06X}", a)} else {"ac".to_string()}));
        self.restore_debug_msg();
    }

    pub fn ora(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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
        
        self.debug();
        self.restore_debug_msg();
    }

    pub fn rol(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("ROL".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord),
            ABS => Some(self.fetch_next_word()),
            ABX => Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord)),
            _ => panic!("Invalid address mode for ROL"),
        };

        let byte = match addr {
            None => &mut self.ac,
            Some(addr) => self.cpu_mem.mut_byte_at(addr),
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shl(1).wrapping_add(if self.ps.test_bit(BitMasks::C) {0b0000_0001} else {0});
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::C, orig_byte & 0b1000_0000 != 0);
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);

        self.debug_imm(format!("{} rotated left ({orig_byte} -> {new_byte})", if let Some(a) = addr {format!("byte at {:#06X}", a)} else {"ac".to_string()}));
        self.restore_debug_msg();
    }

    pub fn ror(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("ROR".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(self.fetch_next_byte() as CPUWord),
            ZPX => Some(self.fetch_next_byte().wrapping_add(self.rx) as CPUWord),
            ABS => Some(self.fetch_next_word()),
            ABX => Some(self.fetch_next_word().wrapping_add(self.rx as CPUWord)),
            _ => panic!("Invalid address mode for ROR"),
        };

        let byte = match addr {
            None => &mut self.ac,
            Some(addr) => self.cpu_mem.mut_byte_at(addr),
        };

        let orig_byte = byte.clone();
        *byte = byte.wrapping_shr(1).wrapping_add(if self.ps.test_bit(BitMasks::C) {0b1000_0000} else {0});
        self.cycles += 1;
        let new_byte = byte.clone();

        self.ps.set_bit(BitMasks::C, orig_byte & 0b0000_0001 != 0);
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, new_byte & 0b1000_0000 != 0);

        self.debug_imm(format!("{} rotated right ({orig_byte} -> {new_byte})", if let Some(a) = addr {format!("byte at {:#06X}", a)} else {"ac".to_string()}));
        self.restore_debug_msg();
    }

    pub fn sbc(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
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
        self.ac = self.ac.wrapping_sub(val + if !self.ps.test_bit(BitMasks::C) {1} else {0});

        self.update_v_flag(orig_val, val, self.ac);
        if self.ps.test_bit(BitMasks::V) {
            self.ps.set_bit(BitMasks::C, false);
        }
        self.ps.set_bit(BitMasks::Z, self.ac == 0);
        self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);

        self.debug();
        self.restore_debug_msg();
    }

    pub fn sta(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("STA".to_string());

        let addr = match mode {
            ZPG => self.fetch_next_byte() as CPUWord,
            ZPX => {
                self.cycles += 1;
                self.fetch_next_byte().wrapping_add(self.rx) as CPUWord
            },
            ABS => self.fetch_next_word(),
            ABX => {
                self.cycles += 1;
                self.fetch_next_word().wrapping_add(self.rx as CPUWord)
            },
            ABY => {
                self.cycles += 1;
                self.fetch_next_word().wrapping_add(self.ry as CPUWord)
            },
            IDX => {
                self.cycles += 1;
                let addr = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.fetch_word_at(addr)
            },
            IDY => {
                let addr = self.fetch_next_byte();
                self.cycles += 1;
                self.fetch_word_at(addr as CPUWord).wrapping_add(self.ry as CPUWord)
            },
            _ => panic!("Invalid address mode for STA"),
        };

        self.debug_imm(format!("store ac ({:#04X}) at {addr:#06X}", self.ac));
        *self.cpu_mem.mut_byte_at(addr) = self.ac;
        self.cycles += 1;

        self.debug();
        self.restore_debug_msg();
    }

    pub fn stx(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("STX".to_string());

        let addr = match mode {
            ZPG => self.fetch_next_byte() as CPUWord,
            ZPY => {
                let addr = self.fetch_next_byte().wrapping_add(self.ry) as CPUWord;
                self.cycles += 1;
                addr
            },
            ABS => self.fetch_next_word(),
            _ => panic!("Invalid address mode for STX"),
        };

        self.debug_imm(format!("store rx ({:#04X}) at {addr:#06X}", self.rx));
        *self.cpu_mem.mut_byte_at(addr) = self.ac;
        self.cycles += 1;

        self.debug();
        self.restore_debug_msg();
    }

    pub fn sty(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;
        self.push_debug_msg("STY".to_string());

        let addr = match mode {
            ZPG => self.fetch_next_byte() as CPUWord,
            ZPX => {
                let addr = self.fetch_next_byte().wrapping_add(self.rx) as CPUWord;
                self.cycles += 1;
                addr
            },
            ABS => self.fetch_next_word(),
            _ => panic!("Invalid address mode for STY"),
        };

        self.debug_imm(format!("store ry ({:#04X}) at {addr:#06X}", self.ry));
        *self.cpu_mem.mut_byte_at(addr) = self.ac;
        self.cycles += 1;

        self.debug();
        self.restore_debug_msg();
    }
}