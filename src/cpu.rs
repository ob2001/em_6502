use crate::prelude::*;

/// 56 spec instructions, +1 added instruction for debugging
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum CPUInstruction {
    ADC(CPUAddrMode),
    AND(CPUAddrMode),
    ASL(CPUAddrMode),
    BCC(CPUAddrMode),
    BCS(CPUAddrMode),
    BEQ(CPUAddrMode),
    BIT(CPUAddrMode),
    BMI(CPUAddrMode),
    BNE(CPUAddrMode),
    BPL(CPUAddrMode),
    BRK(CPUAddrMode),
    BVC(CPUAddrMode),
    BVS(CPUAddrMode),
    CLC(CPUAddrMode),
    CLD(CPUAddrMode),
    CLI(CPUAddrMode),
    CLV(CPUAddrMode),
    CMP(CPUAddrMode),
    CPX(CPUAddrMode),
    CPY(CPUAddrMode),
    DEC(CPUAddrMode),
    DEX(CPUAddrMode),
    DEY(CPUAddrMode),
    EOR(CPUAddrMode),

    /// Not part of cpu spec. Added to halt execution for testing
    HLT(CPUAddrMode),

    INC(CPUAddrMode),
    INX(CPUAddrMode),
    INY(CPUAddrMode),
    JMP(CPUAddrMode),
    JSR(CPUAddrMode),
    LDA(CPUAddrMode),
    LDX(CPUAddrMode),
    LDY(CPUAddrMode),
    LSR(CPUAddrMode),
    NOP(CPUAddrMode),
    ORA(CPUAddrMode),
    PHA(CPUAddrMode),
    PHP(CPUAddrMode),
    PLA(CPUAddrMode),
    PLP(CPUAddrMode),
    ROL(CPUAddrMode),
    ROR(CPUAddrMode),
    RTI(CPUAddrMode),
    RTS(CPUAddrMode),
    SBC(CPUAddrMode),
    SEC(CPUAddrMode),
    SED(CPUAddrMode),
    SEI(CPUAddrMode),
    STA(CPUAddrMode),
    STX(CPUAddrMode),
    STY(CPUAddrMode),
    TAX(CPUAddrMode),
    TAY(CPUAddrMode),
    TSX(CPUAddrMode),
    TXA(CPUAddrMode),
    TXS(CPUAddrMode),
    TYA(CPUAddrMode),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum CPUAddrMode {
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
    IDX,

    /// 4 ?+ 1 cycles
    IDY,
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
    cycles: usize,

    /// Contains all memory the cpu can access, ordered as cpu expects:
    /// - Page 0 (0x0000 - 0x00FF): Zero page memory
    /// - Page 1 (0x0100 - 0x01FF): Stack memory
    /// - 0xFFFA - 0xFFFB: Index (abs) of non-maskable interrupt handler
    /// - 0xFFFC - 0xFFFD: Index (abs) of power on reset location
    /// - 0xFFFE - 0xFFFF: Index (abs) of BRK/interrupt request handler
    cpu_mem: [CPUByte; CPU_MEMSIZE],

    dbg: bool,
    debug_msg: Vec<String>,
}

/// CPU creation/destruction, do not interact with runtime
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
            cycles: 0,
            cpu_mem: [0; CPU_MEMSIZE],
            dbg: false,
            debug_msg: vec![],
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
        self.cycles = 0;
    }
}

/// CPU internal runtime functions (most affect cycle count)
impl CPU6502 {
    /// Reset the cpu and set the program counter to the address stored in the power-on index memory location
    pub fn power_on(&mut self) {
        self.reset();
        self.push_debug_msg("power_on".to_string());
        self.debug_imm("reset (0 cycles)".to_string());

        self.pc = self.get_next_word();

        self.debug_imm("ret -> pc".to_string());

        self.clear_debug_msg();
    }

    /// Reset the cpu to power-on state before starting to run
    pub fn power_on_and_run(&mut self, debugging: bool) {
        self.dbg = debugging;

        self.power_on();

        self.push_debug_msg("run".to_string());
        while self.execute_next_ins() != CPUInstruction::HLT(CPUAddrMode::IMP) { };
        self.restore_debug_msg();

        self.debug_imm("cpu halted".to_string());
    }

    /// Run without resetting cpu to power-on state
    pub fn run_as_is(&mut self, debugging: bool) {
        self.dbg = debugging;

        self.push_debug_msg("run_as_is".to_string());
        while self.execute_next_ins() != CPUInstruction::HLT(CPUAddrMode::IMP) { };
        self.restore_debug_msg();

        self.debug_imm("cpu halted".to_string());
    }

    /// 1 cycle
    pub fn get_next_byte(&mut self) -> CPUByte {
        self.debug_imm("get_next_byte".to_string());

        let ret = self.cpu_mem[self.pc as usize];
        self.cycles += 1;
        self.pc += 1;

        self.debug_ret_byte("get_next_byte", ret);
        
        ret
    }

    /// 2 cycles
    pub fn get_next_word(&mut self) -> CPUWord {
        self.debug_imm("get_next_word => low".to_string());
        
        let low = self.cpu_mem[self.pc as usize];
        self.cycles += 1;
        self.pc += 1;

        self.debug_imm("get_next_word => high".to_string());
        
        let high = self.cpu_mem[self.pc as usize];
        self.cycles += 1;
        self.pc += 1;
        
        let ret = (high as u16) << 8 + low as u16;
        self.debug_ret_word("get_next_word", ret);
        
        ret
    }

    /// 1 cycle
    pub fn get_byte_zp(&mut self, addr: CPUByte) -> CPUByte {
        self.debug_imm("get_byte_zp".to_string());
        
        let ret = self.cpu_mem[(0x0000 + addr as CPUWord) as usize];
        self.cycles += 1;
        self.debug_ret_byte("get_byte_zp", ret);
        
        ret
    }

    /// 1 cycle
    pub fn get_byte_abs(&mut self, addr: CPUWord) -> CPUByte {
        self.debug_imm("get_byte_abs".to_string());
        
        self.cycles += 1;
        let  ret = self.cpu_mem[addr as usize];
        self.debug_ret_byte("get_byte_abs", ret);
        
        ret
    }

    /// 2 cycles
    pub fn get_word_zp(&mut self, addr: CPUByte) -> CPUWord {
        self.push_debug_msg("get_word_zp".to_string());
        self.debug();
        
        let low = self.get_byte_zp(addr);
        let high = self.get_byte_zp(addr + 1);
        let ret = ((high as u16) << 8) + low as u16;
        self.debug_ret_word("get_word_zp", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    pub fn get_word_abs(&mut self, addr: CPUWord) -> CPUWord {
        self.push_debug_msg("get_word_abs".to_string());
        self.debug();

        let low = self.get_byte_abs(addr);
        let high = self.get_byte_abs(addr + 1);
        let ret = ((high as u16) << 8) + low as u16;
        self.debug_ret_word("get_word_abs", ret);

        self.restore_debug_msg();
        ret
    }

    /// 1 cycle
    pub fn add_bytes_wrap(&mut self, a: CPUByte, b: CPUByte) -> CPUByte {
        self.debug_imm("add_bytes_wrap".to_string());

        let ret = a.wrapping_add(b);
        self.cycles += 1;
        self.debug_ret_byte("add_bytes_wrap", ret);

        ret
    }

    /// 1 cycle
    pub fn add_words_wrap(&mut self, a: CPUWord, b: CPUWord) -> CPUWord {
        self.debug_imm("add_words_wrap".to_string());

        let ret = a.wrapping_add(b);
        self.cycles += 1;
        self.debug_ret_word("add_words_wrap", ret);

        ret
    }

    /// 1 cycle
    pub fn imm(&mut self) -> CPUByte {
        self.push_debug_msg("get_imm".to_string());
        
        let ret = self.get_next_byte();
        self.debug();
        self.debug_ret_byte("get_imm", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    pub fn zpg(&mut self) -> CPUByte {
        self.push_debug_msg("get_zpg".to_string());
        
        let addr = self.get_next_byte();
        let ret = self.get_byte_zp(addr);
        self.debug();
        self.debug_ret_byte("get_zpg", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    pub fn zpx(&mut self) -> CPUByte {
        self.push_debug_msg("get_zpx".to_string());
        
        let addr = self.get_next_byte();
        let addr = self.add_bytes_wrap(addr, self.rx);
        let ret = self.get_byte_zp(addr);
        self.debug();
        self.debug_ret_byte("get_zpx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    pub fn zpy(&mut self) -> CPUByte {
        self.push_debug_msg("get_zpy".to_string());
        
        let addr = self.get_next_byte();
        let addr = self.add_bytes_wrap(addr, self.ry);
        let ret = self.get_byte_zp(addr);
        self.debug();
        self.debug_ret_byte("get_zpy", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    pub fn abs(&mut self) -> CPUByte {
        self.push_debug_msg("get_abs".to_string());
        
        let addr = self.get_next_word();
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_abs", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 ?+ 1 cycles
    pub fn abx(&mut self) -> CPUByte {
        self.push_debug_msg("get_abx".to_string());
        
        let tmp_addr = self.get_next_word();
        let addr = tmp_addr + self.rx as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_abx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 ?+ 1 cycles
    pub fn aby(&mut self) -> CPUByte {
        self.push_debug_msg("get_aby".to_string());
        
        let tmp_addr = self.get_next_word();
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_aby", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 5 cycles
    pub fn idx(&mut self) -> CPUByte {
        self.push_debug_msg("get_inx".to_string());
        
        let tmp_addr = self.get_next_byte();
        let tmp_addr = self.add_bytes_wrap(tmp_addr, self.rx);
        let addr = self.get_word_zp(tmp_addr);
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_inx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 4 ?+ 1 cycles
    pub fn idy(&mut self) -> CPUByte {
        self.push_debug_msg("get_iny".to_string());
        
        let tmp_addr = self.get_next_byte();
        let tmp_addr = self.get_word_zp(tmp_addr);
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.get_byte_abs(addr);
        self.debug();
        self.debug_ret_byte("get_iny", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    pub fn push_byte(&mut self, val: CPUByte) {       
        self.push_debug_msg("push_byte".to_string());

        self.cpu_mem[(0x0100 + self.sp as u16) as usize] = val;
        self.cycles += 1;
        self.debug_imm(format!("push val ({:#04x})", val));

        // Don't need to check is stack overflows. Will likely crash process, this is correct behaviour.
        self.sp = self.sp.wrapping_add(1);
        self.cycles += 1;
        self.debug_imm("inc sp".to_string());

        self.restore_debug_msg();
    }

    /// 3 cycles
    pub fn pull_byte(&mut self) -> CPUByte {
        // Don't need to check if stack underflows. Will likely crash process, this is correct behaviour.
        self.push_debug_msg("pull_byte".to_string());

        self.sp = self.sp.wrapping_sub(1);
        self.cycles += 1;
        self.debug_imm("dec sp".to_string());

        let ret = self.cpu_mem[(0x0100 + self.sp as u16) as usize];
        self.cycles += 2;
        self.debug_imm(format!("retrieve val ({:#04x}", ret));

        self.debug_ret_byte("pull_byte", ret);
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    pub fn push_word(&mut self, val: CPUWord) {
        let [high, low] = val.to_be_bytes();
        
        self.push_debug_msg(format!("push_word => store low ({:#04x})", val));
        self.push_byte(low);
        self.cycles -= 1;
        self.debug();

        self.restore_debug_msg();

        self.push_debug_msg(format!("push_word => store high ({:#04x})", val));
        self.push_byte(high);
        self.cycles -= 1;
        self.debug();

        self.restore_debug_msg();
    }

    /// 3 cycles
    pub fn pull_word(&mut self) -> CPUWord {        
        self.push_debug_msg("pull_word".to_string());
        
        self.push_debug_msg("high".to_string());
        let high = self.pull_byte();
        self.cycles -= 1;
        self.debug();
        self.restore_debug_msg();

        self.push_debug_msg("low".to_string());
        let low = self.pull_byte();
        self.cycles -= 1;
        self.debug();
        self.restore_debug_msg();
        
        let ret = ((high as CPUWord) << 8) + low as CPUWord;
        self.cycles -= 1;
        self.debug();

        self.debug_ret_word("pull_word", ret);

        self.restore_debug_msg();
        ret
    }

    /// 0 cycles
    pub fn check_zero(&mut self, val: CPUByte) {
        if val == 0 {
            self.set_zero();
        } else {
            self.unset_zero();
        }

        self.debug_imm(format!("check_zero ({:#04x})", val))
    }

    /// 0 cycles
    pub fn check_negative(&mut self, val: CPUByte) {
        if val & 0b1000_0000 != 0 {
            self.set_negative();
        } else {
            self.unset_negative();
        }

        self.debug_imm(format!("check_negative ({:#04x} / {:#010b})", val, val))
    }

    /// 0 cycles
    pub fn carry_if(&mut self, cond: bool) {
        if cond {
            self.set_carry()
        } else {
            self.unset_carry();
        }

        self.debug_imm(format!("carry_if ({})", cond));
    }

    /// 0 cycles
    pub fn check_overflow(&mut self, init_val: CPUByte, add_val: CPUByte, final_val: CPUByte) {
        let init_val_is_negative = init_val & 0b1000_0000 != 0;
        let add_val_is_negative = add_val & 0b1000_0000 != 0;
        let final_val_is_negative = final_val & 0b1000_0000 != 0;

        if (init_val_is_negative && add_val_is_negative && !final_val_is_negative) || (!init_val_is_negative && !add_val_is_negative && final_val_is_negative){
            self.set_overflow()
        } else {
            self.unset_overflow();
        }

        self.debug_imm(format!("check_overflow (init_val: {:#010b} add_val: {:#010b} final_val: {:#010b})", init_val, add_val, final_val))
    }
}

/// CPU instruction decoding and execution
impl CPU6502 {
    /// 1 cycle
    pub fn decode_next_ins(&mut self) -> CPUInstruction {
        use CPUInstruction::*;
        use CPUAddrMode::*;

        self.push_debug_msg("decode_next_ins".to_string());
        let opcode = self.get_next_byte();

        let ret = match opcode {
            0x00 => BRK(IMP),
            0x01 => ORA(IDX),
            0x05 => ORA(ZPG),
            0x06 => ASL(ZPG),
            0x08 => PHP(IMP),
            0x09 => ORA(IMM),
            0x0A => ASL(ACC),
            0x0D => ORA(ABS),
            0x0E => ASL(ABS),
            0x10 => BPL(REL),
            0x11 => ORA(IDY),
            0x15 => ORA(ZPX),
            0x18 => CLC(IMP),
            0x19 => ORA(ABY),
            0x1D => ORA(ABX),
            0x1E => ASL(ABX),
            0x16 => ASL(ZPX),
            0x20 => JSR(ABS),
            0x21 => AND(IDX),
            0x24 => BIT(ZPG),
            0x25 => AND(ZPG),
            0x26 => ROL(ZPG),
            0x28 => PLP(IMP),
            0x29 => AND(IMM),
            0x2A => ROL(ACC),
            0x2C => BIT(ABS),
            0x2D => AND(ABS),
            0x2E => ROL(ABS),
            0x30 => BMI(REL),
            0x31 => AND(IDY),
            0x35 => AND(ZPX),
            0x36 => ROL(ZPX),
            0x38 => SEC(IMP),
            0x39 => AND(ABY),
            0x3D => AND(ABX),
            0x3E => ROL(ABX),
            0x40 => RTI(IMP),
            0x41 => EOR(IDX),
            0x45 => EOR(ZPG),
            0x46 => LSR(ZPG),
            0x48 => PHA(IMP),
            0x49 => EOR(IMM),
            0x4A => LSR(ACC),
            0x4C => JMP(ABS),
            0x4D => EOR(ABS),
            0x4E => LSR(ABS),
            0x50 => BVC(REL),
            0x51 => EOR(IDY),
            0x55 => EOR(ZPX),
            0x56 => LSR(ZPX),
            0x58 => CLI(IMP),
            0x59 => EOR(ABY),
            0x5D => EOR(ABX),
            0x5E => LSR(ABX),
            0x60 => RTS(IMP),
            0x61 => ADC(IDX),
            0x65 => ADC(ZPG),
            0x66 => ROR(ZPG),
            0x68 => PLA(IMP),
            0x69 => ADC(IMM),
            0x6A => ROR(ACC),
            0x6C => JMP(IND),
            0x6D => ADC(ABS),
            0x6E => ROR(ABS),
            0x70 => BVS(REL),
            0x71 => ADC(IDY),
            0x75 => ADC(ZPX),
            0x76 => ROR(ZPX),
            0x78 => SEI(IMP),
            0x79 => ADC(ABY),
            0x7D => ADC(ABX),
            0x7E => ROR(ABX),
            0x81 => STA(IDX),
            0x84 => STY(ZPG),
            0x85 => STA(ZPG),
            0x86 => STX(ZPG),
            0x88 => DEY(IMP),
            0x8A => TXA(IMP),
            0x8C => STY(ABS),
            0x8D => STA(ABS),
            0x8E => STX(ABS),
            0x90 => BCC(REL),
            0x91 => STA(IDY),
            0x94 => STY(ZPX),
            0x95 => STA(ZPX),
            0x96 => STX(ZPY),
            0x98 => TYA(IMP),
            0x99 => STA(ABY),
            0x9A => TXS(IMP),
            0x9D => STA(ABX),
            0xA0 => LDY(IMM),
            0xA1 => LDA(IDX),
            0xA2 => LDX(IMM),
            0xA4 => LDY(ZPG),
            0xA5 => LDA(ZPG),
            0xA6 => LDX(ZPG),
            0xA8 => TAY(IMP),
            0xA9 => LDA(IMM),
            0xAA => TAX(IMP),
            0xAC => LDY(ABS),
            0xAD => LDA(ABS),
            0xAE => LDX(ABS),
            0xB0 => BCS(REL),
            0xB1 => LDA(IDY),
            0xB4 => LDY(ZPX),
            0xB5 => LDA(ZPX),
            0xB6 => LDX(ZPY),
            0xB8 => CLV(IMP),
            0xB9 => LDA(ABY),
            0xBA => TSX(IMP),
            0xBC => LDY(ABX),
            0xBD => LDA(ABX),
            0xBE => LDX(ABY),
            0xC0 => CPY(IMM),
            0xC1 => CMP(IDX),
            0xC4 => CMP(ZPG),
            0xC5 => CMP(ZPG),
            0xC6 => DEC(ZPG),
            0xC8 => INY(IMP),
            0xC9 => CMP(IMM),
            0xCA => DEX(IMP),
            0xCC => CPY(ABS),
            0xCD => CMP(ABS),
            0xCE => DEC(ABS),
            0xD0 => BNE(REL),
            0xD1 => CMP(IDY),
            0xD5 => CMP(ZPX),
            0xD6 => DEC(ZPX),
            0xD8 => CLD(IMP),
            0xD9 => CMP(ABY),
            0xDD => CMP(ABX),
            0xDE => DEC(ABX),
            0xE0 => CPX(IMM),
            0xE1 => SBC(IDX),
            0xE4 => CPX(ZPG),
            0xE5 => SBC(ZPG),
            0xE6 => INC(ZPG),
            0xE8 => INX(IMP),
            0xE9 => SBC(IMM),
            0xEA => NOP(IMP),
            0xEC => CPX(ABS),
            0xED => SBC(ABS),
            0xEE => INC(ABS),
            0xF0 => BEQ(REL),
            0xF1 => SBC(IDY),
            0xF5 => SBC(ZPX),
            0xF6 => INC(ZPX),
            0xF8 => SED(IMP),
            0xF9 => SBC(ABY),
            0xFD => SBC(ABX),
            0xFE => INC(ABX),
            
            // Non-spec/illegal instruction codes
            0xFF => HLT(IMP),
            _ => panic!("Invalid opcode: {:#04x}", opcode),
        };

        self.debug();
        self.debug_ret_ins(ret);

        self.restore_debug_msg();
        ret
    }

    /// Takes as many cycles as the instruction being executed
    pub fn execute_next_ins(&mut self) -> CPUInstruction {
        use CPUInstruction::*;
        use CPUAddrMode::*;

        self.push_debug_msg("execute_next_command".to_string());

        let ins = self.decode_next_ins();

        match ins {
            ADC(mode) => self.adc(mode),
            AND(mode) => self.and(mode),
            ASL(mode) => self.asl(mode),
            // ! todo: add debug information
            BCC(REL) => {
                self.cycles += 1;
                if !self.is_carry() {
                    self.cycles += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycles += 2;
                    }
                }
            },
            // ! todo: add debug information
            BCS(REL) => {
                self.cycles += 1;
                if self.is_carry() {
                    self.cycles += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycles += 2;
                    }
                }
            },
            // ! todo: add debug information
            BEQ(REL) => {
                self.cycles += 1;
                if self.is_zero() {
                    self.cycles += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycles += 2;
                    }
                }
            },
            BIT(mode) => self.bit(mode),
            // ! todo: add debug information
            BMI(REL) => {
                self.cycles += 1;
                if self.is_negative() {
                    self.cycles += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycles += 2;
                    }
                }
            },
            // ! todo: add debug information
            BNE(REL) => {
                self.cycles += 1;
                if !self.is_zero() {
                    self.cycles += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycles += 2;
                    }
                }
            },
            // ! todo: add debug information
            BPL(REL) => {
                self.cycles += 1;
                if !self.is_negative() {
                    self.cycles += 1;
                    let orig = self.pc;
                    self.pc = self.pc.wrapping_add_signed((self.get_next_byte() as i8).into());

                    if self.pc / 256 != orig / 256 {
                        self.cycles += 2;
                    }
                }
            },
            BRK(IMP) => {
                self.push_debug_msg("BRK".to_string());

                self.push_debug_msg("push_pc".to_string());
                self.push_word(self.pc);
                self.restore_debug_msg();

                self.push_debug_msg("push_ps".to_string());
                self.push_byte(self.ps);
                self.restore_debug_msg();
                
                self.set_break_command();

                self.push_debug_msg("jmp_to_loc_at_irq_vector".to_string());
                self.pc = self.get_word_abs(0xFFFE);
                self.restore_debug_msg();

                self.debug();
                self.restore_debug_msg();
            },
            // !
            // ! todo: Continue instruction implementation
            // !
            HLT(IMP) => { self.cycles += 1; self.debug_imm("HLT".to_string())},
            LDA(mode) => self.lda(mode),
            _ => panic!("Unimplemented instruction: {:?}", ins),
        }
        self.restore_debug_msg();

        ins
    }
}

/// CPU instruction implementations
impl CPU6502 {
    /// 1 - 5 cycles
    pub fn adc(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;

        self.push_debug_msg("ADC".to_string());

        let init_val = self.ac;
        let add_val = if self.is_carry() {1} else {0} + match mode {
            IMM => self.imm(),
            ZPG => self.zpg(),
            ZPX => self.zpx(),
            ABS => self.abs(),
            ABX => self.abx(),
            ABY => self.aby(),
            IDX => self.idx(),
            IDY => self.idy(),
            _ => panic!("Invalid address mode for ADC"),
        };
        
        self.ac += add_val;
        
        self.check_overflow(init_val, add_val, self.ac);
        self.carry_if(self.is_overflow());
        self.check_negative(self.ac);
        self.check_zero(self.ac);

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
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

        self.check_zero(self.ac);
        self.check_negative(self.ac);

        self.restore_debug_msg();
    }

    /// 1 - 6 cycles
    pub fn asl(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;

        self.push_debug_msg("ASL".to_string());

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
        self.cycles += 1;

        self.check_zero(val);
        self.check_negative(val);

        match mode {
            ACC => { self.ac = val },
            _ => { self.cpu_mem[addr.unwrap() as usize] = val },
        };

        self.restore_debug_msg();
    }

    /// 2 - 3 cycles
    pub fn bit(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;

        self.push_debug_msg("BIT".to_string());

        let val = match mode {
            ZPG => self.zpg(),
            ABS => self.abs(),
            _ => panic!("Invalid address mode for BIT")
        };

        self.cycles += 1;

        self.check_zero(self.ac & val);
        self.check_negative(val);

        if val & 0b0100_0000 != 0 {
            self.set_overflow();
        } else {
            self.unset_overflow();
        }

        self.restore_debug_msg();
    }

    /// 1 - 5 cycles
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

        self.debug_imm("ret -> ac".to_string());

        self.check_zero(self.ac);
        self.check_negative(self.ac);

        self.restore_debug_msg();
    }
}

/// CPU utility functions (cycle count unaffected)
impl CPU6502 {
    /// 0 cycles
    pub fn is_carry(&self) -> bool {
        self.ps & 0b0000_0001 != 0
    }

    /// 0 cycles
    pub fn is_zero(&self) -> bool {
        self.ps & 0b0000_0010 != 0
    }

    /// 0 cycles
    pub fn is_interrupt_disable(&self) -> bool {
        self.ps & 0b0000_0100 != 0
    }

    /// 0 cycles
    pub fn is_decimal_mode(&self) -> bool {
        self.ps & 0b0000_1000 != 0
    }

    /// 0 cycles
    pub fn is_break_command(&self) -> bool {
        self.ps & 0b0001_0000 != 0
    }

    /// 0 cycles
    pub fn is_overflow(&self) -> bool {
        self.ps & 0b0100_0000 != 0
    }

    /// 0 cycles
    pub fn is_negative(&self) -> bool {
        self.ps & 0b1000_0000 != 0
    }

    /// 0 cycles
    pub fn reset_status_flags(&mut self) {
        self.ps = 0b0000_0000;
    }

    /// 0 cycles
    pub fn set_carry(&mut self) {
        self.ps |= 0b0000_0001;
    }

    /// 0 cycles
    pub fn set_zero(&mut self) {
        self.ps |= 0b0000_0010;
    }

    /// 0 cycles
    pub fn set_interrupt_disable(&mut self) {
        self.ps |= 0b0000_0100;
    }

    /// 0 cycles
    pub fn set_decimal_mode(&mut self) {
        self.ps |= 0b0000_1000;
    }

    /// 0 cycles
    pub fn set_break_command(&mut self) {
        self.ps |= 0b0001_0000;
    }

    /// 0 cycles
    pub fn set_overflow(&mut self) {
        self.ps |= 0b0100_0000;
    }

    /// 0 cycles
    pub fn set_negative(&mut self) {
        self.ps |= 0b1000_0000;
    }

    /// 0 cycles
    pub fn unset_carry(&mut self) {
        self.ps &= 0b1111_1110;
    }

    /// 0 cycles
    pub fn unset_zero(&mut self) {
        self.ps &= 0b1111_1101;
    }

    /// 0 cycles
    pub fn unset_interrupt_disable(&mut self) {
        self.ps &= 0b1111_1011;
    }

    /// 0 cycles
    pub fn unset_decimal_mode(&mut self) {
        self.ps &= 0b1111_0111;
    }

    /// 0 cycles
    pub fn unset_break_command(&mut self) {
        self.ps &= 0b1110_1111;
    }

    /// 0 cycles
    pub fn unset_overflow(&mut self) {
        self.ps &= 0b1011_1111;
    }

    /// 0 cycles
    pub fn unset_negative(&mut self) {
        self.ps &= 0b0111_1111;
    }
}

/// CPU debugging utility functions (cycle count unaffected)
impl CPU6502 {
    /// Allows user to switch on or off cpu debugging messages.
    /// 
    /// *(Beware of unintended side effects if modifying value while cpu is running.)*
    pub fn set_dbg_mode(&mut self, dbg: bool) {
        self.dbg = dbg;
    }

    /// Print current state of cpu with a header message detailing the cpu execution stack
    pub fn debug(&self) {
        if self.dbg {
            print!("*** ");
            for m in self.debug_msg.iter().take(self.debug_msg.len() - 1) {
                print!("{} => ", m);
            }
            print!("{}", self.debug_msg.last().unwrap());
            println!(" ***\n{}\n", self);
        }
    }
    
    /// Print the name and return value of a cpu function returning a CPUByte
    pub fn debug_ret_byte(&self, fname: &'static str, ret_val: CPUByte) {
        if self.dbg {
            println!("-> {}: {:#04X}\n", fname, ret_val);
        }
    }

    /// Print the name and return value of a cpu function returning a CPUWord
    pub fn debug_ret_word(&self, fname: &'static str, ret_val: CPUWord) {
        if self.dbg {
            println!("-> {}: {:#06X}\n", fname, ret_val);
        }
    }

    /// Print the instruction returned by decode_next_ins
    pub fn debug_ret_ins(&self, ret_ins: CPUInstruction) {
        if self.dbg {
            println!("-> decode_next_ins: {:?}\n", ret_ins);
        }
    }

    /// Reset the debug_msg vector to empty
    pub fn clear_debug_msg(&mut self) {
        if self.dbg {
            self.debug_msg = vec![];
        }
    }

    /// Push `msg` onto `self.debug_msg`
    pub fn push_debug_msg(&mut self, msg: String) {
        if self.dbg {
            self.debug_msg.push(msg);
        }
    }

    /// Pop and discard most recently added `msg` from `self.debug_msg`
    pub fn restore_debug_msg(&mut self) {
        if self.dbg {
            let _ = self.debug_msg.pop();
        }
    }

    /// Allow for quick, one-time debug prints with a custom message suffix
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
        write!(f, "cycles: {}\npc: {:#06X}\nnext byte: {:#04X}\nnext word: {:#06X}\nps (NVuBDIZC): {:08b}\nsp: {:#04X}\nac: {:#04X}\nrx: {:#04X}\nry: {:#04X}",
            self.cycles,
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
            write!(f, "{:02X}, ", byte)?;
        }

        std::fmt::Result::Ok(())
    }
}