use crate::cpu::{CPU6502, CPUByte, CPUWord, CPUInstruction};

/// CPU runtime debugging utility functions (No CPU cycles)
impl CPU6502 {
    /// Allows user to switch on or off CPU debugging messages.
    /// 
    /// *(Beware of unintended side effects if modifying value while CPU is running.)*
    pub fn set_dbg_mode(&mut self, dbg: bool) {
        self.dbg = dbg;
    }

    /// Print current state of CPU with a header message detailing the CPU execution stack.
    pub fn debug(&self) {
        if self.dbg {
            print!("*** ");
            for m in self.debug_msg.iter().take(self.debug_msg.len() - 1) {
                print!("{m} => ");
            }
            print!("{}", self.debug_msg.last().unwrap());
            println!(" ***\n{}\n", self);
        }
    }
    
    /// Print the name and return value of a CPU function returning a CPUByte.
    pub fn debug_ret_byte(&self, fname: &'static str, ret_val: CPUByte) {
        if self.dbg {
            println!("-> {fname}: {ret_val:#04X}\n");
        }
    }

    /// Print the name and return value of a CPU function returning a CPUWord.
    pub fn debug_ret_word(&self, fname: &'static str, ret_val: CPUWord) {
        if self.dbg {
            println!("-> {fname}: {ret_val:#06X}\n");
        }
    }

    /// Print the instruction returned by decode_next_ins.
    pub fn debug_ret_ins(&self, ret_ins: CPUInstruction) {
        if self.dbg {
            println!("-> decode_next_ins: {ret_ins:?}\n");
        }
    }

    /// Reset the debug_msg vector to be empty.
    pub fn clear_debug_msg(&mut self) {
        if self.dbg {
            self.debug_msg = vec![];
        }
    }

    /// Push `msg` onto `self.debug_msg`.
    pub fn push_debug_msg(&mut self, msg: String) {
        if self.dbg {
            self.debug_msg.push(msg);
        }
    }

    /// Pop and discard most recently added `msg` from `self.debug_msg`.
    pub fn restore_debug_msg(&mut self) {
        if self.dbg {
            let _ = self.debug_msg.pop();
        }
    }

    /// Allow for quick, one-time debug prints with a custom message suffix.
    pub fn debug_imm(&mut self, msg: String) {
        if self.dbg {
            self.push_debug_msg(msg);
            self.debug();
            self.restore_debug_msg();
        }
    }
}

impl std::fmt::Display for CPU6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cycles: {}\npc: {:#06X}\nps (NVuBDIZC): {:08b}\nsp: {:#04X}\nac: {:#04X}\nrx: {:#04X}\nry: {:#04X}\n-----------------\nbyte[pc]: {:#04X}\nins[pc]: {}\nword[pc]: {:#06X}\n\nbyte[sp]: {:#04X}\nins[sp]: {}\nword[sp]: {:#06X}\n-----------------",
            self.cycles,
            self.pc,
            self.ps.to_inner(),
            self.sp,
            self.ac,
            self.rx,
            self.ry,
            self.cpu_mem.byte_at(self.pc),
            if let Ok(ins) = self.decode(self.cpu_mem.byte_at(self.pc)) {format!("{ins:?}")} else {"None".to_string()},
            ((self.cpu_mem.byte_at(self.pc.wrapping_add(1)) as CPUWord) << 8) + self.cpu_mem.byte_at(self.pc) as CPUWord,
            self.cpu_mem.byte_at(0x0100 + (self.sp.wrapping_add(1)) as CPUWord),
            if let Ok(ins) = self.decode(self.cpu_mem.byte_at(0x0100 + (self.sp.wrapping_add(1)) as CPUWord)) {format!("{ins:?}")} else {"None".to_string()},
            ((self.cpu_mem.byte_at(0x0100 + self.sp as CPUWord) as CPUWord) << 8) + self.cpu_mem.byte_at(0x0100 + self.sp.wrapping_add(1) as CPUWord) as CPUWord,
        )
    }
}

impl std::fmt::Debug for CPU6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:?}", self, self.cpu_mem)
    }
}