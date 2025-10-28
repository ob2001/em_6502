/// ~153 instructions
pub mod cpu_ins {
    use super::CPUByte;

    pub const LDA_IMM: CPUByte = 0xA9;
    pub const LDA_ZPG: CPUByte = 0xA5;
    pub const LDA_ZPX: CPUByte = 0xB5;
    pub const LDA_ABS: CPUByte = 0xAD;
    pub const LDA_ABX: CPUByte = 0xBD;
    pub const LDA_ABY: CPUByte = 0xB9;
    pub const LDA_INX: CPUByte = 0xA1;
    pub const LDA_INY: CPUByte = 0xB1;

    pub const LDX_IMM: CPUByte = 0xA2;
    pub const LDX_ZPG: CPUByte = 0xA6;
    pub const LDX_ZPY: CPUByte = 0xB6;
    pub const LDX_ABS: CPUByte = 0xAE;
    pub const LDX_ABY: CPUByte = 0xBE;

    pub const LDY_IMM: CPUByte = 0xA0;
    pub const LDY_ZPG: CPUByte = 0xA4;
    pub const LDY_ZPX: CPUByte = 0xB4;
    pub const LDY_ABS: CPUByte = 0xAC;
    pub const LDY_ABX: CPUByte = 0xBC;

    pub const STA_ZPG: CPUByte = 0x85;
    pub const STA_ZPX: CPUByte = 0x95;
    pub const STA_ABS: CPUByte = 0x8D;
    pub const STA_ABX: CPUByte = 0x9D;
    pub const STA_ABY: CPUByte = 0x99;
    pub const STA_INX: CPUByte = 0x81;
    pub const STA_INY: CPUByte = 0x91;

    pub const STX_ZPG: CPUByte = 0x86;
    pub const STX_ZPY: CPUByte = 0x96;
    pub const STX_ABS: CPUByte = 0x8E;

    pub const STY_ZPG: CPUByte = 0x85;
    pub const STY_ZPX: CPUByte = 0x95;
    pub const STY_ABS: CPUByte = 0x8C;

    pub const TAX_IMP: CPUByte = 0xAA;

    pub const TAY_IMP: CPUByte = 0xA8;

    pub const TXA_IMP: CPUByte = 0x8A;

    pub const TYA_IMP: CPUByte = 0x98;

    pub const TSX_IMP: CPUByte = 0xBA;

    pub const TXS_IMP: CPUByte = 0x9A;

    pub const PHA_IMP: CPUByte = 0x48;

    pub const PHP_IMP: CPUByte = 0x08;

    pub const PLA_IMP: CPUByte = 0x68;

    pub const PLP_IMP: CPUByte = 0x28;

    pub const AND_IMM: CPUByte = 0x29;
    pub const AND_ZPG: CPUByte = 0x25;
    pub const AND_ZPX: CPUByte = 0x35;
    pub const AND_ABS: CPUByte = 0x2D;
    pub const AND_ABX: CPUByte = 0x3D;
    pub const AND_ABY: CPUByte = 0x39;
    pub const AND_INX: CPUByte = 0x21;
    pub const AND_INY: CPUByte = 0x31;

    pub const EOR_IMM: CPUByte = 0x49;
    pub const EOR_ZPG: CPUByte = 0x45;
    pub const EOR_ZPX: CPUByte = 0x55;
    pub const EOR_ABS: CPUByte = 0x4D;
    pub const EOR_ABX: CPUByte = 0x5D;
    pub const EOR_ABY: CPUByte = 0x59;
    pub const EOR_INX: CPUByte = 0x41;
    pub const EOR_INY: CPUByte = 0x51;

    pub const ORA_IMM: CPUByte = 0x09;
    pub const ORA_ZPG: CPUByte = 0x05;
    pub const ORA_ZPX: CPUByte = 0x15;
    pub const ORA_ABS: CPUByte = 0x0D;
    pub const ORA_ABX: CPUByte = 0x1D;
    pub const ORA_ABY: CPUByte = 0x19;
    pub const ORA_INX: CPUByte = 0x01;
    pub const ORA_INY: CPUByte = 0x11;

    pub const BIT_ZPG: CPUByte = 0x24;
    pub const BIT_ABS: CPUByte = 0x2C;

    pub const ADC_IMM: CPUByte = 0x69;
    pub const ADC_ZPG: CPUByte = 0x65;
    pub const ADC_ZPX: CPUByte = 0x75;
    pub const ADC_ABS: CPUByte = 0x6D;
    pub const ADC_ABX: CPUByte = 0x7D;
    pub const ADC_ABY: CPUByte = 0x79;
    pub const ADC_INX: CPUByte = 0x61;
    pub const ADC_INY: CPUByte = 0x71;

    pub const SBC_IMM: CPUByte = 0xE9;
    pub const SBC_ZPG: CPUByte = 0xE5;
    pub const SBC_ZPX: CPUByte = 0xF5;
    pub const SBC_ABS: CPUByte = 0xED;
    pub const SBC_ABX: CPUByte = 0xFD;
    pub const SBC_ABY: CPUByte = 0xF9;
    pub const SBC_INX: CPUByte = 0xE1;
    pub const SBC_INY: CPUByte = 0xF1;

    pub const CMP_IMM: CPUByte = 0xC9;
    pub const CMP_ZPG: CPUByte = 0xC5;
    pub const CMP_ZPX: CPUByte = 0xD5;
    pub const CMP_ABS: CPUByte = 0xCD;
    pub const CMP_ABX: CPUByte = 0xDD;
    pub const CMP_ABY: CPUByte = 0xD9;
    pub const CMP_INX: CPUByte = 0xC1;
    pub const CMP_INY: CPUByte = 0xD1;

    pub const CPX_IMM: CPUByte = 0xE0;
    pub const CPX_ZPG: CPUByte = 0xE4;
    pub const CPX_ABS: CPUByte = 0xEC;

    pub const CPY_IMM: CPUByte = 0xC0;
    pub const CPY_ZPG: CPUByte = 0xC4;
    pub const CPY_ABS: CPUByte = 0xCC;

    pub const INC_ZPG: CPUByte = 0xE6;
    pub const INC_ZPX: CPUByte = 0xF6;
    pub const INC_ABS: CPUByte = 0xEE;
    pub const INC_ABX: CPUByte = 0xFE;

    pub const INX_IMP: CPUByte = 0xE8;

    pub const INY_IMP: CPUByte = 0xC8;

    pub const DEC_ZPG: CPUByte = 0xC6;
    pub const DEC_ZPX: CPUByte = 0xD6;
    pub const DEC_ABS: CPUByte = 0xCE;
    pub const DEC_ABX: CPUByte = 0xDE;

    pub const DEX_IMP: CPUByte = 0xCA;

    pub const DEY_IMP: CPUByte = 0x88;

    pub const ASL_ACC: CPUByte = 0x0A;
    pub const ASL_ZPG: CPUByte = 0x06;
    pub const ASL_ZPX: CPUByte = 0x16;
    pub const ASL_ABS: CPUByte = 0x0E;
    pub const ASL_ABX: CPUByte = 0x1E;

    pub const LSR_ACC: CPUByte = 0x4A;
    pub const LSR_ZPG: CPUByte = 0x46;
    pub const LSR_ZPX: CPUByte = 0x56;
    pub const LSR_ABS: CPUByte = 0x4E;
    pub const LSR_ABX: CPUByte = 0x5E;

    pub const ROL_ACC: CPUByte = 0x2A;
    pub const ROL_ZPG: CPUByte = 0x26;
    pub const ROL_ZPX: CPUByte = 0x36;
    pub const ROL_ABS: CPUByte = 0x2E;
    pub const ROL_ABX: CPUByte = 0x3E;

    pub const ROR_ACC: CPUByte = 0x6A;
    pub const ROR_ZPG: CPUByte = 0x66;
    pub const ROR_ZPX: CPUByte = 0x76;
    pub const ROR_ABS: CPUByte = 0x6E;
    pub const ROR_ABX: CPUByte = 0x7E;

    pub const JMP_ABS: CPUByte = 0x4C;
    pub const JMP_IND: CPUByte = 0x6C;

    pub const JSR_ABS: CPUByte = 0x20;

    pub const RTS_IMP: CPUByte = 0x60;

    pub const BCC_REL: CPUByte = 0x90;

    pub const BCS_REL: CPUByte = 0xB0;

    pub const BEQ_REL: CPUByte = 0xF0;

    pub const BMI_REL: CPUByte = 0x30;

    pub const BNE_REL: CPUByte = 0xD0;

    pub const BPL_REL: CPUByte = 0x10;

    pub const BVC_REL: CPUByte = 0x50;

    pub const BVS_REL: CPUByte = 0x70;

    pub const CLC_IMP: CPUByte = 0x18;

    pub const CLD_IMP: CPUByte = 0xD8;

    pub const CLI_IMP: CPUByte = 0x58;

    pub const CLV_IMP: CPUByte = 0xB8;

    pub const SEC_IMP: CPUByte = 0x38;

    pub const SED_IMP: CPUByte = 0xF8;

    pub const SEI_IMP: CPUByte = 0x78;

    pub const BRK_IMP: CPUByte = 0x00;

    pub const NOP_IMP: CPUByte = 0xEA;

    pub const RTI_IMP: CPUByte = 0x40;

    /// Not part of official spec. Added to allow emulation termination while testing.
    pub const HLT_IMP: CPUByte = 0xFF;
}

pub enum AddrMode {
    /// (1 or 0?) cycles
    IMP,

    /// (1 or 0?) cycles
    ACC,

    /// 1 cycle
    IMM,

    /// 2 cycles
    ZPG,

    /// 3 cycles
    ZPX,

    /// 3 cycles
    ZPY,

    /// (1?) cycle
    REL,

    /// 3 cycles
    ABS,

    /// 3 ?+ 1 cycles
    ABX,

    /// 3 ?+ 1 cycles
    ABY,

    /// (?) cycles
    IND,

    /// 5 cycles
    INX,

    /// 4 ?+ 1 cycles
    INY,
}

pub type CPUByte = u8;
pub type CPUWord = u16;

pub const CPU_MEMSIZE: usize = 65536;

/// Return new zeroed cpu memory.
pub fn mem_zeros() -> [CPUByte; CPU_MEMSIZE] {
    [0; CPU_MEMSIZE]
}

/// Return new zeroed cpu memory with `code` inserted in range `mem[idx]` - `mem[idx + code.len()`.
pub fn mem_with_code_at(code: &[CPUByte], idx: usize) -> Result<[CPUByte; CPU_MEMSIZE], &'static str> {
    let mut ret = mem_zeros();
    if idx <= CPU_MEMSIZE && code.len() + idx <= CPU_MEMSIZE {
        for (offset, &byte) in code.iter().enumerate() {
            ret[idx + offset] = byte;
        }
        Ok(ret)
    } else if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    }
}

/// Modify existing cpu memory `mem` by inserting `code` in range `mem[idx]` - `mem[idx + code.len()]`.
///
/// Will overwrite memory in modification range.
pub fn mem_insert_code_at(mem: &mut [CPUByte; CPU_MEMSIZE], code: &[CPUByte], idx: usize) -> Result<(), &'static str> {
    if idx <= CPU_MEMSIZE && code.len() + idx <= CPU_MEMSIZE {
        for (offset, &byte) in code.iter().enumerate() {
            mem[idx + offset] = byte;
        }
        Ok(())
    } else if idx > CPU_MEMSIZE {
        Err("Error: code insertion point out of memory bounds")
    } else {
        Err("Error: code overflows memory bounds. Consider inserting at an earlier index")
    }
}

pub struct CPU6502 {
    /// Program counter
    /// 
    /// Points to the next instruction to be executed. The value
    /// of the program counter is modified automatically as instructions
    /// are executed.
    /// 
    /// The value of the program counter can be modified by executing a jump,
    /// a relative branch or a subroutine call to another memory address or
    /// by returning from a subroutine or interrupt.
    pc: CPUWord,

    /// Stack pointer
    /// 
    /// Points to cpu memory range 0x0100 - 0x01ff which is used as the cpu's stack.
    /// The location of the stack is fixed and cannot be moved.
    /// 
    /// Pushin bytes to the stack causes the stack pointer to be decremented. Conversely
    /// popping bytes causes it to be incremented.
    /// 
    /// The CPU does not detect if the stack is overflowed by excessive pushing or
    /// popping operations and will most likely result in the error crashing.
    sp: CPUByte,

    /// Accumulator
    /// 
    /// The 8 bit accumulator is used in all arithmetic and logical operations (with the
    /// exception of increments and decrements). The contents of the accumulator can be
    /// stored and retrieved either from memory or the stack.
    /// 
    /// Most complex operations will need to use the accumulator for arithmetic and
    /// efficient optimisation of its use is a key feature of time critical routines.
    ac: CPUByte,

    /// Index register X
    /// 
    /// The 8 bit index register is most commonly used to hold counters or offsets for
    /// accessing memory. The value of the X register can be loaded and saved in memory,
    /// compared with values held in memory or incremented and decremented.
    /// 
    /// The X register has one special function. It can be used to get a copy of the stack
    /// pointer or change its value.
    rx: CPUByte,

    /// Index register Y
    /// 
    /// The Y register is similar to the X register in that it is available for holding counter
    /// or offsets memory access and supports the same set of memory load, save and compare
    /// operations as well as increments and decrements. It has no special functions.
    ry: CPUByte,

    /// Processor status (bitfield):
    /// - Bit 0: Carry flag: The carry flag is set if the last operation caused an overflow from bit 7 of the result or an underflow from bit 0. This condition is set during arithmetic, comparison and during logical shifts. It can be explicitly set using the "Set Carry Flag" (SEC) instruction and cleared with "Clear Carry Flag" (CLC).
    /// - Bit 1: Zero flag: The zero flag is set if the result of the last operation was zero.
    /// - Bit 2: Interrupt disable: The interrupt disable flag is set if the program has executed a "Set Interrupt Disable" (SEI) instruction. While this flag is set the processor will not respond to interrupts from devices until it is cleared by a "Clear Interrupt Disable" (CLI) instruction.
    /// - Bit 3: Decimal mode: While the decimal mode flag is set the processor will obey the rules of Binary Coded Decimal (BCD) arithmetic during addition and subtraction.
    /// - Bit 4: Break command: The break command bit is set when a BRK instruction has been executed and an interrupt has been generated to process it.
    /// - Bit 5: Overflow flag: Set during arithmetic operations if the result has yielded an invalid 2's complement result (e.g. adding two positive numbers nd ending up with a negative result). It is determined by looking at the carry between bits 6 and 7 and between bit 7 and the carry flag.
    /// - Bit 6: Negative flag: Set if the result of the last operation had bit 7 set to one.
    ps: CPUByte,

    /// Counter incremented to emulate cpu clock cycles
    cycle_count: usize,

    /// Contains all memory the cpu can access, ordered as cpu expects:
    /// - Page 0 (0x0000 - 0x00FF): Zero page memory
    /// - Page 1 (0x0100 - 0x01FF): Stack memory
    /// - 0xFFFA - 0xFFFB: Index (abs) of non-maskable interrupt handler
    /// - 0xFFFC - 0xFFFD: Index (abs) of power on reset location
    /// - 0xFFFE - 0xFFFF: Index (abs) of BRK/interrupt request handler
    cpu_mem: [CPUByte; CPU_MEMSIZE],

    dbg: bool,
    debug_msg: String,
}

/// Creation/destruction functions, do not interact with runtime
impl CPU6502 {
    /// Create a new CPU6502 with registers set to defaults and zeroed memory
    pub fn new() -> Self {
        CPU6502 { 
            pc: 0xFFFC,
            sp: 0,
            ac: 0,
            rx: 0,
            ry: 0,
            ps: 0,
            cycle_count: 0,
            cpu_mem: [0; CPU_MEMSIZE],
            dbg: false,
            debug_msg: String::from(""),
        }
    }

    /// Create a new CPU6502 with memory as specified in `mem`
    pub fn new_with_mem(mem: [CPUByte; CPU_MEMSIZE]) -> Self {
        let mut cpu = Self::new();
        cpu.flash_mem(mem);
        cpu
    }

    /// Set the contents of the cpu memory to `mem`
    pub fn flash_mem(&mut self, mem: [CPUByte; CPU_MEMSIZE]) {
        self.cpu_mem = mem;
    }

    /// Put the cpu in the standard power-on/reset state
    pub fn reset(&mut self) {
        self.pc = 0xFFFC;
        self.sp = 0;
        self.ac = 0;
        self.rx = 0;
        self.ry = 0;
        self.ps = 0;
        self.cycle_count = 0;
    }
}

/// Runtime functions (most affect cycle count)
impl CPU6502 {
    /// Reset the cpu and set the program counter to the address stored in the power-on index memory location
    pub fn power_on(&mut self) {
        self.reset();
        self.set_debug_msg("power_on => reset (0 cycles)".to_string());
        self.debug();
        self.set_debug_msg("power_on".to_string());
        self.pc = self.get_next_word();
        self.set_debug_msg("power_on => ret -> pc".to_string());
        self.debug();
    }

    /// Reset the cpu to power-on state before starting to run
    pub fn power_on_and_run(&mut self, dbg: bool) {
        self.dbg = dbg;

        self.power_on();
        self.set_debug_msg("run".to_string());

        while self.execute_next_ins() != cpu_ins::HLT_IMP {};

        self.set_debug_msg("cpu halted".to_string());
        self.debug();
    }

    /// Run without resetting cpu to power-on state
    pub fn run_as_is(&mut self, dbg: bool) {
        self.dbg = dbg;

        self.set_debug_msg("run_as_is".to_string());
        while self.execute_next_ins() != cpu_ins::HLT_IMP {
            self.set_debug_msg("execute_next_ins".to_string());
            self.debug() 
        };
        self.set_debug_msg("cpu halted".to_string());
        self.debug();
    }

    /// 1 cycle
    pub fn get_next_byte(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_next_byte".to_string());
        
        let ret = self.cpu_mem[self.pc as usize];
        self.cycle_count += 1;
        self.pc += 1;
        self.debug();
        
        self.debug_ret_byte("get_next_byte", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 2 cycles
    pub fn get_next_word(&mut self) -> CPUWord {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_next_word => low".to_string());
        self.debug();
        self.set_debug_msg(orig_debug_msg.clone());
        
        let low = self.cpu_mem[self.pc as usize];
        self.cycle_count += 1;
        self.pc += 1;

        self.append_debug_msg("get_next_word => high".to_string());
        self.debug();
        
        let high = self.cpu_mem[self.pc as usize];
        self.cycle_count += 1;
        self.pc += 1;
        
        let ret = (high as u16) << 8 + low as u16;
        self.debug_ret_word("get_next_word", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 1 cycle
    pub fn get_byte_zp(&mut self, addr: CPUByte) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_byte_zp".to_string());
        self.debug();
        
        let ret = self.cpu_mem[(0x0000 + addr as CPUWord) as usize];
        self.cycle_count += 1;
        self.debug_ret_byte("get_byte_zp", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 1 cycle
    pub fn get_byte_abs(&mut self, addr: CPUWord) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_byte_abs".to_string());
        self.debug();
        
        self.cycle_count += 1;
        let  ret = self.cpu_mem[addr as usize];
        self.debug_ret_byte("get_byte_abs", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 2 cycles
    pub fn get_word_zp(&mut self, addr: CPUByte) -> CPUWord {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_word_zp".to_string());
        self.debug();
        
        let low = self.get_byte_zp(addr);
        let high = self.get_byte_zp(addr + 1);
        let ret = ((high as u16) << 8) + low as u16;
        self.debug_ret_word("get_word_zp", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 2 cycles
    pub fn get_word_abs(&mut self, addr: CPUWord) -> CPUWord {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_word_abs".to_string());
        self.debug();

        let low = self.get_byte_abs(addr);
        let high = self.get_byte_abs(addr + 1);
        let ret = ((high as u16) << 8) + low as u16;
        self.debug_ret_word("get_word_abs", ret);

        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 1 cycle
    pub fn add_bytes_wrap(&mut self, a: CPUByte, b: CPUByte) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("add_bytes_wrap".to_string());
        self.debug();

        let ret = a.wrapping_add(b);
        self.cycle_count += 1;
        self.debug_ret_byte("add_bytes_wrap", ret);

        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 1 cycle
    pub fn add_words_wrap(&mut self, a: CPUWord, b: CPUWord) -> CPUWord {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("add_words_wrap".to_string());
        self.debug();

        let ret = a.wrapping_add(b);
        self.cycle_count += 1;
        self.debug_ret_word("add_words_wrap", ret);

        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 1 cycle
    pub fn imm(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_imm".to_string());
        
        let ret = self.get_next_byte();
        self.debug();
        self.debug_ret_byte("get_imm", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 2 cycles
    pub fn zpg(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_zpg".to_string());
        
        let addr = self.get_next_byte();
        let ret = self.get_byte_zp(addr);
        self.debug();
        self.debug_ret_byte("get_zpg", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 3 cycles
    pub fn zpx(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_zpx".to_string());
        
        let addr = self.get_next_byte();
        let addr = self.add_bytes_wrap(addr, self.rx);
        let ret = self.get_byte_zp(addr);
        self.debug();
        self.debug_ret_byte("get_zpx", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 3 cycles
    pub fn zpy(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_zpy".to_string());
        
        let addr = self.get_next_byte();
        let addr = self.add_bytes_wrap(addr, self.ry);
        let ret = self.get_byte_zp(addr);
        self.debug();
        self.debug_ret_byte("get_zpy", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 3 cycles
    pub fn abs(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_abs".to_string());
        
        let addr = self.get_next_word();
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_abs", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 3 ?+ 1 cycles
    pub fn abx(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_abx".to_string());
        
        let tmp_addr = self.get_next_word();
        let addr = tmp_addr + self.rx as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycle_count += 1;
        }
        
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_abx", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 3 ?+ 1 cycles
    pub fn aby(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_aby".to_string());
        
        let tmp_addr = self.get_next_word();
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycle_count += 1;
        }
        
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_aby", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 5 cycles
    pub fn inx(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_inx".to_string());
        
        let tmp_addr = self.get_next_byte();
        let tmp_addr = self.add_bytes_wrap(tmp_addr, self.rx);
        let addr = self.get_word_zp(tmp_addr);
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_inx", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 4 ?+ 1 cycles
    pub fn iny(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("get_iny".to_string());
        
        let tmp_addr = self.get_next_byte();
        let tmp_addr = self.get_word_zp(tmp_addr);
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycle_count += 1;
        }
        
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_iny", ret);
        
        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 2 cycles
    pub fn push_byte(&mut self, val: CPUByte) {
        let orig_debug_msg = self.debug_msg.clone();
        
        self.append_debug_msg(format!("push_byte => store val ({:#04x})", val));
        self.cpu_mem[(0x0100 + self.sp as u16) as usize] = val;
        self.cycle_count += 1;
        self.debug();
        self.set_debug_msg(orig_debug_msg.clone());

        // Don't need to check is stack overflows. Will likely crash process, this is correct behaviour.
        self.append_debug_msg("push_byte => inc sp".to_string());
        self.sp = self.sp.wrapping_add(1);
        self.cycle_count += 1;
        self.debug();

        self.set_debug_msg(orig_debug_msg);
    }

    /// 3 cycles
    pub fn pull_byte(&mut self) -> CPUByte {
        let orig_debug_msg = self.debug_msg.clone();

        // Don't need to check if stack underflows. Will likely crash process, this is correct behaviour.
        self.append_debug_msg("pull_byte => dec sp".to_string());
        self.sp = self.sp.wrapping_sub(1);
        self.cycle_count += 1;
        self.debug();
        self.set_debug_msg(orig_debug_msg.clone());

        let ret = self.cpu_mem[(0x0100 + self.sp as u16) as usize];
        self.cycle_count += 2;
        self.append_debug_msg(format!("pull_byte => retrieve val ({:#04x})", ret));
        self.debug();

        self.debug_ret_byte("pull_byte", ret);

        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 2 cycles
    pub fn push_word(&mut self, val: CPUWord) {
        let orig_debug_msg = self.debug_msg.clone();

        let [high, low] = val.to_be_bytes();
        
        self.append_debug_msg(format!("push_word => store low ({:#04x})", val));
        self.push_byte(low);
        self.cycle_count -= 1;
        self.debug();

        self.set_debug_msg(orig_debug_msg.clone());

        self.append_debug_msg(format!("push_word => store high ({:#04x})", val));
        self.push_byte(high);
        self.cycle_count -= 1;
        self.debug();

        self.set_debug_msg(orig_debug_msg);
    }

    /// 3 cycles
    pub fn pull_word(&mut self) -> CPUWord {
        let orig_debug_msg = self.debug_msg.clone();
        
        self.append_debug_msg("pull_word => high".to_string());
        let high = self.pull_byte();
        self.cycle_count -= 1;
        self.debug();
        self.set_debug_msg(orig_debug_msg.clone());

        self.append_debug_msg("pull_word => low".to_string());
        let low = self.pull_byte();
        self.cycle_count -= 1;
        self.debug();
        self.set_debug_msg(orig_debug_msg.clone());
        
        let ret = ((high as CPUWord) << 8) + low as CPUWord;
        self.cycle_count -= 1;
        self.debug();

        self.debug_ret_word("pull_word", ret);

        self.set_debug_msg(orig_debug_msg);
        ret
    }

    /// 0 cycles
    pub fn check_zero(&mut self, val: CPUByte) {
        if val == 0 {
            self.set_zero();
        } else {
            self.unset_zero();
        }

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg(format!("check_zero ({:#04x})", val));
        self.debug();
        self.set_debug_msg(orig_debug_msg);
    }

    /// 0 cycles
    pub fn check_negative(&mut self, val: CPUByte) {
        if val & 0b1000_0000 != 0 {
            self.set_negative();
        } else {
            self.unset_negative();
        }

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg(format!("check_negative ({:#04x} / {:#010b})", val, val));
        self.debug();
        self.set_debug_msg(orig_debug_msg);
    }

    /// 0 cycles
    pub fn carry_if(&mut self, cond: bool) {
        if cond {
            self.set_carry()
        } else {
            self.unset_carry();
        }

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg(format!("carry_if ({})", cond));
        self.debug();
        self.set_debug_msg(orig_debug_msg);
    }

    /// 0 cycles
    pub fn check_overflow(&mut self, init_pos: bool, val: CPUByte) {
        if (init_pos && val & 0b1000_0000 != 0) || (!init_pos && val & 0b1000_0000 == 0) {
            self.set_overflow()
        } else {
            self.unset_overflow();
        }

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg(format!("check_overflow (init_pos: {} val: {:#04x} / {:#010b})", init_pos, val, val));
        self.debug();
        self.set_debug_msg(orig_debug_msg);
    }

    /// 1 - 5 cycles
    pub fn adc(&mut self, mode: AddrMode) {
        use AddrMode::*;

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("adc".to_string());

        let init_pos = self.ac & 0b1000_0000 == 0;
        
        self.ac += if self.is_carry() {1} else {0} + match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            INX => self.inx(),
            INY => self.iny(),
            _ => panic!("Invalid address mode for ADC"),
        };
        
        self.check_overflow(init_pos, self.ac);
        self.carry_if(self.is_overflow());
        self.check_negative(self.ac);
        self.check_zero(self.ac);

        self.set_debug_msg(orig_debug_msg);
    }

    /// 1 - 5 cycles
    pub fn and(&mut self, mode: AddrMode) {
        use AddrMode::*;

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("and".to_string());

        self.ac &= match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            INX => self.inx(),
            INY => self.iny(),
            _ => panic!("Invalid address mode for AND"),
        };

        self.check_zero(self.ac);
        self.check_negative(self.ac);

        self.set_debug_msg(orig_debug_msg);
    }

    /// 1 - 6 cycles
    pub fn asl(&mut self, mode: AddrMode) {
        use AddrMode::*;

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("asl".to_string());

        let addr = match mode {
            ACC => None,
            ZPG => Some(0x0000 + self.get_next_byte() as CPUWord),
            ZPX => Some(0x0000 + self.get_next_byte() as CPUWord),
            ABS => Some(self.get_next_word()),
            ABX => Some(self.get_next_word()),
            _ => panic!("Invalid address mode for ASL"),
        };

        let mut val = match addr {
            None => self.ac,
            Some(a) => self.cpu_mem[a as usize],
        };

        self.carry_if(val & 0b1000_0000 != 0);

        val <<= 1;
        self.cycle_count += 1;

        self.check_zero(val);
        self.check_negative(val);

        match mode {
            ACC => { self.ac = val },
            _ => { self.cpu_mem[addr.unwrap() as usize] = val },
        };

        self.set_debug_msg(orig_debug_msg);
    }

    /// 2 - 3 cycles
    // ! todo: add debug information
    pub fn bit(&mut self, mode: AddrMode) {
        use AddrMode::*;

        let val = match mode {
            ZPG => self.zpg(),
            ABS => self.abs(),
            _ => panic!("Invalid address mode for BIT")
        };

        self.cycle_count += 1;

        self.check_zero(self.ac & val);
        self.check_negative(val);

        if val & 0b0100_0000 != 0 {
            self.set_overflow();
        } else {
            self.unset_overflow();
        }
    }

    /// 1 - 5 cycles
    pub fn lda(&mut self, mode: AddrMode) {
        use AddrMode::*;

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("lda".to_string());

        self.ac = match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            INX => self.inx(),
            INY => self.iny(),
            _ => panic!("Invalid address mode for LDA"),
        };

        self.append_debug_msg("ret -> ac".to_string());
        self.debug();
        self.set_debug_msg(orig_debug_msg.clone());
        self.append_debug_msg("ret".to_string());

        self.check_zero(self.ac);
        self.check_negative(self.ac);

        self.set_debug_msg(orig_debug_msg);
    }
}

/// Dedicated impl block for cpu instruction execution
impl CPU6502 {
    /// 1 + (op cycles) cycles
    pub fn execute_next_ins(&mut self) -> CPUByte {
        use cpu_ins::*;
        use AddrMode::*;

        let orig_debug_msg = self.debug_msg.clone();
        self.append_debug_msg("execute_next_command".to_string());

        let ins = self.get_next_byte();
        match ins {
            // ADC
            ADC_IMM => self.adc(IMM),
            ADC_ZPG => self.adc(ZPG),
            ADC_ZPX => self.adc(ZPX),
            ADC_ABS => self.adc(ABS),
            ADC_ABX => self.adc(ABX),
            ADC_ABY => self.adc(ABY),
            ADC_INX => self.adc(INX),
            ADC_INY => self.adc(INY),

            // AND
            AND_IMM => self.and(IMM),
            AND_ZPG => self.and(ZPG),
            AND_ZPX => self.and(ZPX),
            AND_ABS => self.and(ABS),
            AND_ABX => self.and(ABX),
            AND_ABY => self.and(ABY),
            AND_INX => self.and(INX),
            AND_INY => self.and(INY),

            // ASL
            ASL_ACC => self.asl(ACC),
            ASL_ZPG => self.asl(ZPG),
            ASL_ZPX => self.asl(ZPX),
            ASL_ABS => self.asl(ABS),
            ASL_ABX => self.asl(ABX),

            // BCC
            BCC_REL => {
                self.cycle_count += 1;
                if !self.is_carry() {
                    self.cycle_count += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycle_count += 2;
                    }
                }
            },

            // BCS
            BCS_REL => {
                self.cycle_count += 1;
                if self.is_carry() {
                    self.cycle_count += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycle_count += 2;
                    }
                }
            },

            // BEQ
            BEQ_REL => {
                self.cycle_count += 1;
                if self.is_zero() {
                    self.cycle_count += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycle_count += 2;
                    }
                }
            },

            // BIT
            BIT_ZPG => self.bit(ZPG),
            BIT_ABS => self.bit(ABS),

            // BMI
            BMI_REL => {
                self.cycle_count += 1;
                if self.is_negative() {
                    self.cycle_count += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycle_count += 2;
                    }
                }
            },

            // BNE
            BNE_REL => {
                self.cycle_count += 1;
                if !self.is_zero() {
                    self.cycle_count += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycle_count += 2;
                    }
                }
            },

            // BPL
            BPL_REL => {
                self.cycle_count += 1;
                if !self.is_negative() {
                    self.cycle_count += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycle_count += 2;
                    }
                }
            },

            // BRK
            BRK_IMP => {
                let orig_debug_msg = self.debug_msg.clone();

                self.append_debug_msg("brk => push_pc".to_string());
                self.push_word(self.pc);
                self.set_debug_msg(orig_debug_msg.clone());

                self.append_debug_msg("brk => push_ps".to_string());
                self.push_byte(self.ps);
                self.set_debug_msg(orig_debug_msg.clone());
                
                self.set_break_command();

                self.append_debug_msg("brk => jmp_to_loc_at_irq_vector (0xFFFE)".to_string());
                self.pc = self.get_word_abs(0xFFFE);
                self.debug();

                self.set_debug_msg(orig_debug_msg);
            },
            // !
            // ! todo: Add debug information in conditional branch instructions
            // !
            // ! todo: Continue instruction implementation
            // !
            // LDA
            LDA_IMM => self.lda(IMM),
            LDA_ZPG => self.lda(ZPG),
            LDA_ZPX => self.lda(ZPX),
            LDA_ABS => self.lda(ABS),
            LDA_ABX => self.lda(ABX),
            LDA_ABY => self.lda(ABY),
            LDA_INX => self.lda(INX),
            LDA_INY => self.lda(INY),

            // HLT
            HLT_IMP => { self.cycle_count += 1; },
            e => panic!("Invalid instrucion code: {}", e),
        }
        self.set_debug_msg(orig_debug_msg);
        ins
    }
}

/// Cpu utility functions (none affect cycle count)
impl CPU6502 {
    /// 0 cycles
    pub fn is_carry(&self) -> bool {
        self.ps & 0b00000001 != 0
    }

    /// 0 cycles
    pub fn is_zero(&self) -> bool {
        self.ps & 0b00000010 != 0
    }

    /// 0 cycles
    pub fn is_interrupt_disable(&self) -> bool {
        self.ps & 0b00000100 != 0
    }

    /// 0 cycles
    pub fn is_decimal_mode(&self) -> bool {
        self.ps & 0b00001000 != 0
    }

    /// 0 cycles
    pub fn is_break_command(&self) -> bool {
        self.ps & 0b00010000 != 0
    }

    /// 0 cycles
    pub fn is_overflow(&self) -> bool {
        self.ps & 0b00100000 != 0
    }

    /// 0 cycles
    pub fn is_negative(&self) -> bool {
        self.ps & 0b01000000 != 0
    }

    /// 0 cycles
    pub fn reset_status_flags(&mut self) {
        self.ps = 0b00000000;
    }

    /// 0 cycles
    pub fn set_carry(&mut self) {
        self.ps |= 0b00000001;
    }

    /// 0 cycles
    pub fn set_zero(&mut self) {
        self.ps |= 0b00000010;
    }

    /// 0 cycles
    pub fn set_interrupt_disable(&mut self) {
        self.ps |= 0b00000100;
    }

    /// 0 cycles
    pub fn set_decimal_mode(&mut self) {
        self.ps |= 0b00001000;
    }

    /// 0 cycles
    pub fn set_break_command(&mut self) {
        self.ps |= 0b00010000;
    }

    /// 0 cycles
    pub fn set_overflow(&mut self) {
        self.ps |= 0b00100000;
    }

    /// 0 cycles
    pub fn set_negative(&mut self) {
        self.ps |= 0b01000000;
    }

    /// 0 cycles
    pub fn unset_carry(&mut self) {
        self.ps &= 0b11111110;
    }

    /// 0 cycles
    pub fn unset_zero(&mut self) {
        self.ps &= 0b11111101;
    }

    /// 0 cycles
    pub fn unset_interrupt_disable(&mut self) {
        self.ps &= 0b11111011;
    }

    /// 0 cycles
    pub fn unset_decimal_mode(&mut self) {
        self.ps &= 0b11110111;
    }

    /// 0 cycles
    pub fn unset_break_command(&mut self) {
        self.ps &= 0b11101111;
    }

    /// 0 cycles
    pub fn unset_overflow(&mut self) {
        self.ps &= 0b11011111;
    }

    /// 0 cycles
    pub fn unset_negative(&mut self) {
        self.ps &= 0b10111111;
    }
}

/// Cpu debugging utility functions (none affect cycle count)
impl CPU6502 {
    pub fn set_dbg_mode(&mut self, dbg: bool) {
        self.dbg = dbg;
    }

    /// Print current state of cpu with a header message detailing the cpu execution stack
    pub fn debug(&self) {
        if self.dbg {
            println!("*** {} ***\n{}\n", self.debug_msg, self);
        }
    }
    
    /// Print the name and return value of a cpu function returning a CPUByte
    pub fn debug_ret_byte(&self, fname: &'static str, ret_val: CPUByte) {
        if self.dbg {
            println!("{} -> {:#04x}\n", fname, ret_val);
        }
    }

    /// Print the name and return value of a cpu function returning a CPUWord
    pub fn debug_ret_word(&self, fname: &'static str, ret_val: CPUWord) {
        if self.dbg {
            println!("{} -> {:#06x}\n", fname, ret_val);
        }
    }

    /// Set the `debug()` header message to `msg`
    pub fn set_debug_msg(&mut self, msg: String) {
        if self.dbg {
            self.debug_msg = msg;
        }
    }

    /// Append `" => {msg}"` to `debug()` header message
    pub fn append_debug_msg(&mut self, msg: String) {
        if self.dbg {
            self.debug_msg += &(String::from(" => ") + &msg);
        }
    }
}

impl std::fmt::Display for CPU6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cycle count: {}\npc: {:#06x}\nnext byte: {:#04x}\nnext word: {:#06x}\nps (NOBDIZC): {:07b}\nsp: {:#04x}\nac: {:#04x}\nrx: {:#04x}\nry: {:#04x}",
            self.cycle_count,
            self.pc,
            self.cpu_mem[self.pc as usize],
            ((self.cpu_mem[(self.pc.wrapping_add(1)) as usize] as CPUWord) << 8) + self.cpu_mem[self.pc as usize] as CPUWord,
            self.ps,
            self.sp,
            self.ac,
            self.rx,
            self.ry
        )
    }
}

impl std::fmt::Debug for CPU6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n", self)?;
        for byte in self.cpu_mem {
            write!(f, "{:02x}, ", byte)?;
        }

        std::fmt::Result::Ok(())
    }
}