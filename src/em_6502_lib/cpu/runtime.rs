use super::super::{prelude::*, cpu::CPU6502};

/// CPU internal runtime functions
impl CPU6502 {
    /// Reset the CPU to pwoer-on state, then set the program counter to the address
    /// stored at the Power On Reset memory location (0xFFFC/0xFFFD).
    pub fn power_on(&mut self) {
        self.reset();
        self.clear_debug_msg();

        self.push_debug_msg("power_on".to_string());
        self.debug_imm("reset (0 cycles)".to_string());

        self.pc = self.fetch_next_word();
        self.debug_imm(format!("set_pc ({:#06X})", self.pc));

        self.clear_debug_msg();
    }

    /// Perform CPU reset then begin emulation loop.
    pub fn power_on_and_run(&mut self, debugging: bool) {
        self.dbg = debugging;

        self.power_on();

        self.push_debug_msg("run".to_string());
        loop {
            let ins = self.execute_next_ins();
            match ins {
                Err(e) => {
                    self.clear_debug_msg();
                    self.debug_imm(format!("!!! CPU halting on encountering illegal opcode: {e} !!!"));
                    panic!("\nCPU halt on encountering illegal opcode: {e}.\nTo allow CPU to pass over illegal opcodes, use `cpu.set_illegal_opcode_mode(true)`\n")
                }
                Ok(CPUInstruction::HLT(CPUAddrMode::IMP)) => break,
                Ok(_) => {},
            }

            if self.cycle_limit > 0 && self.cycles >= self.cycle_limit {
                self.clear_debug_msg();
                self.debug_imm("!!! CPU halting on exceeding cycle limit !!!".to_string());
                break;
            }
        };
        self.restore_debug_msg();

        self.debug_imm("!!! CPU halted!!! ".to_string());
    }

    /// Begin CPU emulation loop without performing a reset.
    pub fn run_as_is(&mut self, debugging: bool) {
        self.dbg = debugging;
        self.clear_debug_msg();

        self.push_debug_msg("run_as_is".to_string());
        loop {
            let ins = self.execute_next_ins();
            match ins {
                Err(e) => {
                    self.clear_debug_msg();
                    self.debug_imm(format!("!!! CPU halting on encountering illegal opcode: {e} !!!"));
                    panic!("\nCPU halt on encountering illegal opcode: {e}.\nTo allow CPU to pass over illegal opcodes, use `cpu.set_illegal_opcode_mode(true)`\n")
                }
                Ok(CPUInstruction::HLT(CPUAddrMode::IMP)) => break,
                Ok(_) => {},
            }

            if self.cycle_limit > 0 && self.cycles >= self.cycle_limit {
                self.clear_debug_msg();
                self.debug_imm("!!! CPU halting on exceeding cycle limit !!!".to_string());
                break;
            }
        }
        self.restore_debug_msg();

        self.debug_imm("!!! CPU halted !!!".to_string());
    }

    /// 1 cycle
    /// 
    /// Fetch the CPUByte stored in the memory location pointed to by pc.
    /// 
    /// Increments pc.
    pub fn fetch_next_byte(&mut self) -> CPUByte {
        let ret = self.cpu_mem.byte_at(self.pc);

        self.debug_imm(format!("fetch_next_byte ({ret:#04X})"));
        self.cycles += 1;
        self.pc += 1;

        self.debug_ret_byte("fetch_next_byte", ret);
        
        ret
    }

    /// 2 cycles
    /// 
    /// Fetch 2 CPUBytes starting at the memory location pointed to by pc
    /// and return them as a CPUWord (little endian).
    /// 
    /// Increments pc twice.
    pub fn fetch_next_word(&mut self) -> CPUWord {
        let low = self.cpu_mem.byte_at(self.pc);

        self.debug_imm(format!("fetch_next_word => low ({low:#04X})"));
        self.cycles += 1;
        self.pc += 1;

        let high = self.cpu_mem.byte_at(self.pc);
        
        self.debug_imm(format!("fetch_next_word => high ({high:#04X})"));
        self.cycles += 1;
        self.pc += 1;
        
        let ret = (high as CPUWord) << 8 + low as CPUWord;
        self.debug_ret_word("fetch_next_word", ret);
        
        ret
    }

    /// 1 cycle
    /// 
    /// Fetches the CPUByte stored in the memory location at the provided address.
    /// 
    /// Does not affect pc.
    pub fn fetch_byte_at(&mut self, addr: CPUWord) -> CPUByte {
        self.debug_imm(format!("fetch_byte_at ({addr:#06X})"));
        
        self.cycles += 1;
        let  ret = self.cpu_mem.byte_at(addr);
        self.debug_ret_byte("fetch_byte_at", ret);
        
        ret
    }

    /// 2 cycles
    /// 
    /// Fetch 2 CPUBytes starting at the memory location at the provided address
    /// and return them as a CPUWord (little endian).
    /// 
    /// Does not affect pc.
    pub fn fetch_word_at(&mut self, addr: CPUWord) -> CPUWord {
        self.push_debug_msg(format!("fetch_word_at ({addr:#06X})"));
        self.debug();

        let low = self.fetch_byte_at(addr);
        let high = self.fetch_byte_at(addr + 1);
        let ret = ((high as CPUWord) << 8).wrapping_add(low as CPUWord);
        self.debug_ret_word("fetch_word_at", ret);

        self.restore_debug_msg();
        ret
    }

    /// 1 cycle
    ///
    /// Use Immediate addressing mode to obtain argument for CPU instruction
    pub fn imm(&mut self) -> CPUByte {
        self.push_debug_msg("imm".to_string());
        
        let ret = self.fetch_next_byte();
        self.debug();
        self.debug_ret_byte("imm", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    /// 
    /// Use Zero Page addressing mode to obtain argument for CPU instruction
    pub fn zpg(&mut self) -> CPUByte {
        self.push_debug_msg("zpg".to_string());
        
        let addr = self.fetch_next_byte();
        let ret = self.fetch_byte_at(addr as CPUWord);
        self.debug();
        self.debug_ret_byte("zpg", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    /// 
    /// Use Zero Page,X mode to obtain argument for CPU instruction
    pub fn zpx(&mut self) -> CPUByte {
        self.push_debug_msg("zpx".to_string());
        
        let addr = self.fetch_next_byte().wrapping_add(self.rx);
        self.cycles += 1;
        let ret = self.fetch_byte_at(addr as CPUWord);
        self.debug();
        self.debug_ret_byte("zpx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    /// 
    /// Use Zero Page,Y addressing mode to obtain argument for CPU instruction
    pub fn zpy(&mut self) -> CPUByte {
        self.push_debug_msg("zpy".to_string());
        
        let addr = self.fetch_next_byte().wrapping_add(self.ry);
        self.cycles += 1;
        let ret = self.fetch_byte_at(addr as CPUWord);
        self.debug();
        self.debug_ret_byte("zpy", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    /// 
    /// Use Absolute addressing mode to obtain argument for CPU instruction
    pub fn abs(&mut self) -> CPUByte {
        self.push_debug_msg("abs".to_string());
        
        let addr = self.fetch_next_word();
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("abs", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 ?+ 1 cycles
    /// 
    /// Use Absolute,X addressing mode to obtain argument for CPU instruction
    pub fn abx(&mut self) -> CPUByte {
        self.push_debug_msg("abx".to_string());
        
        let tmp_addr = self.fetch_next_word();
        let addr = tmp_addr + self.rx as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("abx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 ?+ 1 cycles
    /// 
    /// Use Absolute,Y addressing mode to obtain argument for CPU instruction
    pub fn aby(&mut self) -> CPUByte {
        self.push_debug_msg("aby".to_string());
        
        let tmp_addr = self.fetch_next_word();
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("aby", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 5 cycles
    /// 
    /// Use Pre-Indexed Indirect addressing mode to obtain argument for CPU instruction
    pub fn idx(&mut self) -> CPUByte {
        self.push_debug_msg("idx".to_string());
        
        let tmp_addr = self.fetch_next_byte().wrapping_add(self.rx);
        self.cycles += 1;
        let addr = self.fetch_word_at(tmp_addr as CPUWord);
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("idx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 4 ?+ 1 cycles
    /// 
    /// Use Post-Indexed addressing mode to obtain argument for CPU instruction
    pub fn idy(&mut self) -> CPUByte {
        self.push_debug_msg("idy".to_string());
        
        let tmp_addr = self.fetch_next_byte();
        let tmp_addr = self.fetch_word_at(tmp_addr as CPUWord);
        let addr = tmp_addr.wrapping_add(self.ry as CPUWord);
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("idy", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 1 cycle
    /// 
    /// Push the value of the passed CPUByte to the stack at the memory location currently indicated by sp.
    /// 
    /// Decrements sp.
    pub fn push_byte(&mut self, val: CPUByte) {       
        self.push_debug_msg("push_byte".to_string());

        *self.cpu_mem.mut_byte_at(0x0100 + self.sp as CPUWord) = val;
        self.cycles += 1;
        self.debug_imm(format!("push val ({val:#04x})"));

        // Don't need to check is stack overflows. Will likely crash process, this is correct behaviour.
        self.sp = self.sp.wrapping_sub(1);
        self.debug_imm("inc sp".to_string());

        self.restore_debug_msg();
    }

    /// 2 cycles
    /// 
    /// Pulls (pops) the last-pushed CPUByte value from the stack at the memory location indicated by sp (- 1).
    /// 
    /// Increments sp.
    pub fn pull_byte(&mut self) -> CPUByte {
        // Don't need to check if stack underflows. Will likely crash process, this is correct behaviour.
        self.push_debug_msg("pull_byte".to_string());

        self.sp = self.sp.wrapping_add(1);
        self.debug_imm("dec sp".to_string());

        let ret = self.cpu_mem.byte_at(0x0100 + self.sp as CPUWord);
        self.cycles += 2;
        self.debug_imm(format!("retrieve val ({ret:#04x}"));

        self.debug_ret_byte("pull_byte", ret);
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    /// 
    /// Push the value of the passed CPUWord to the stack starting at the memory location currently indicated by sp.
    /// 
    /// Decrements sp twice.
    pub fn push_word(&mut self, val: CPUWord) {
        let [high, low] = val.to_be_bytes();
        
        self.push_debug_msg(format!("push_word => store low ({val:#04x})"));
        self.push_byte(low);
        self.debug();

        self.restore_debug_msg();

        self.push_debug_msg(format!("push_word => store high ({val:#04x})"));
        self.push_byte(high);
        self.debug();

        self.restore_debug_msg();
    }

    /// 4 cycles
    /// 
    /// Pulls (pops) the two last-pushed CPUByte values from the stack starting 
    /// at the memory location indicated by sp (- 1).
    /// 
    /// Increments sp twice.
    pub fn pull_word(&mut self) -> CPUWord {        
        self.push_debug_msg("pull_word".to_string());
        
        self.push_debug_msg("high".to_string());
        let high = self.pull_byte();
        self.debug();
        self.restore_debug_msg();

        self.push_debug_msg("low".to_string());
        let low = self.pull_byte();
        self.debug();
        self.restore_debug_msg();
        
        let ret = ((high as CPUWord) << 8) + low as CPUWord;
        self.debug();

        self.debug_ret_word("pull_word", ret);

        self.restore_debug_msg();
        ret
    }

    /// 0 cycles
    /// 
    /// Updates the zero flag based on the CPUByte value passed.
    pub fn update_z(&mut self, val: CPUByte) {
        self.ps.set_bit(BitMasks::Z, val == 0);

        self.debug_imm(format!("check_zero ({val:#04x})"))
    }

    /// 0 cycles
    /// 
    /// Updates the negative flag based on the CPUByte value passed.
    pub fn update_n(&mut self, val: CPUByte) {
        self.ps.set_bit(BitMasks::N, val & 0b1000_0000 != 0);

        self.debug_imm(format!("check_negative ({val:#04x} / {val:#010b})"))
    }

    /// 0 cycles
    /// 
    /// Updates the overflow flag based on the two input CPUByte values and the
    /// output CPUByte value. (Indicates an incorrect 2's complement result from an
    /// arithmetic operation).
    pub fn update_v(&mut self, val1: CPUByte, val2: CPUByte, out: CPUByte) {
        let init_val_is_negative = val1 & 0b1000_0000 != 0;
        let add_val_is_negative = val2 & 0b1000_0000 != 0;
        let final_val_is_negative = out & 0b1000_0000 != 0;

        self.ps.set_bit(BitMasks::V, (init_val_is_negative && add_val_is_negative && !final_val_is_negative) || (!init_val_is_negative && !add_val_is_negative && final_val_is_negative));

        self.debug_imm(format!("check_overflow (init_val: {val1:#010b} add_val: {val2:#010b} final_val: {out:#010b})"))
    }
}

