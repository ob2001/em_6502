use crate::{prelude::*, mem::Mem, bitfield::BitField};

pub mod debug;
pub mod dec_exec;
pub mod ins;
pub mod runtime;

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

    /// Not part of CPU spec. Added to halt execution for testing
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
    /// Points to CPU memory range 0x0100 - 0x01ff which is used as the CPU's stack.
    /// The location of the stack is fixed and cannot be moved.
    /// 
    /// Pushing bytes to the stack causes the stack pointer to be decremented. Conversely
    /// pulling (popping) bytes causes it to be incremented.
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
    /// compared with values held in memory, or incremented and decremented.
    /// 
    /// The X register has one special function. It can be used to get a copy of the stack
    /// pointer or change its value.
    rx: CPUByte,

    /// Index register Y
    /// 
    /// The Y register is similar to the X register in that it is available for holding counter
    /// or offsets memory access and supports the same set of memory load, save, and compare
    /// operations as well as increments and decrements. It has no special functions.
    ry: CPUByte,

    /// Processor status (bitfield):
    /// - Bit 0: Carry flag: The carry flag is set if the last operation caused an overflow from
    /// bit 7 of the result or an underflow from bit 0. This condition is set during arithmetic,
    /// comparison, and during logical shifts. It can be explicitly set using the "Set Carry Flag"
    /// (SEC) instruction and cleared with "Clear Carry Flag" (CLC).
    /// - Bit 1: Zero flag: The zero flag is set if the result of the last operation was zero.
    /// - Bit 2: Interrupt disable: The interrupt disable flag is set if the program has executed
    /// a "Set Interrupt Disable" (SEI) instruction. While this flag is set the processor will not
    /// respond to interrupts from devices until it is cleared by a "Clear Interrupt Disable" (CLI) instruction.
    /// - Bit 3: Decimal mode: While the decimal mode flag is set the processor will obey the rules
    /// of Binary Coded Decimal (BCD) arithmetic during addition and subtraction.
    /// - Bit 4: Break command: The break command bit is set when a BRK instruction has been executed
    /// and an interrupt has been generated to process it.
    /// - Bit 5: Unused.
    /// - Bit 6: Overflow flag: Set during arithmetic operations if the result has yielded an invalid
    /// 2's complement result (e.g. adding two positive numbers and ending up with a negative result).
    /// It is determined by looking at the carry between bits 6 and 7 and between bit 7 and the carry flag.
    /// - Bit 7: Negative flag: Set if the result of the last operation had bit 7 set to one.
    ps: BitField,

    /// Counter incremented to emulate CPU clock cycles
    cycles: usize,

    /// Contains all memory the CPU can access, ordered as CPU expects:
    /// - Page 0 (0x0000 - 0x00FF): Zero page memory
    /// - Page 1 (0x0100 - 0x01FF): Stack memory
    /// - 0xFFFA/0xFFFB: Location of non-maskable interrupt handler
    /// - 0xFFFC/0xFFFD: Location of power on/reset location
    /// - 0xFFFE/0xFFFF: Location of BRK/interrupt request handler
    cpu_mem: Mem,

    dbg: bool,
    debug_msg: Vec<String>,

    /// Defaults to `true`.
    /// 
    /// When `allow_hlt == false` CPU will treat HLT opcode (0xFF) as any other illegal opcode.
    /// 
    /// When `allow_hlt == true` CPU will treat HLT opcode as a legal opcode and function accordingly.
    allow_hlt: bool,

    /// Defaults to `true`.
    /// 
    /// When `illegal_opcode_mode == false` CPU will panic on encountering any illegal opcode 
    /// (except HLT if separately enabled).
    /// 
    /// When `illegal_opcode_mode == true` CPU will perform a NOP on encountering an illegal opcode
    /// (including HLT unless it's been separately enabled)
    illegal_opcode_mode: bool,

    /// Defaults to 0.
    /// 
    /// Halt CPU before decoding next instruction if cycle_count >= cycle_limit.
    /// 
    /// Set cycle_limit to 0 to disable this behaviour.
    cycle_limit: usize,
}

/// 56 spec instructions, +1 added instruction for debugging
/// CPU creation/setup functions, do not interact with runtime (No CPU cycles)
impl CPU6502 {
    /// Create a new CPU6502 with registers set to defaults and zeroed memory
    pub fn new() -> Self {
        CPU6502 { 
            pc: 0xFFFC,
            sp: 255,
            ac: 0,
            rx: 0,
            ry: 0,
            ps: BitField::new(0),
            cycles: 0,
            cpu_mem: Mem::new_nops(),
            dbg: false,
            debug_msg: vec![],
            allow_hlt: true,
            illegal_opcode_mode: true,
            cycle_limit: 0,
        }
    }

    /// Create a new CPU6502 with memory as specified in `mem`
    pub fn new_with_mem(mem: Mem) -> Self {
        let mut cpu = Self::new();
        cpu.flash_mem(mem);
        cpu
    }

    pub fn new_with_mem_from_file(mem_file: String) -> Result<Self, String> {
        if let Ok(mem) = Mem::new_from_file(mem_file.clone()) {
            Ok(Self::new_with_mem(mem))
        } else {
            Err(format!("Error loading or parsing memory file: {mem_file}"))
        }
    }

    /// Sets the `allow_hlt` field of the CPU.
    /// 
    /// When `allow_hlt == false` CPU will treat HLT opcode (0xFF) as any other illegal opcode.
    /// 
    /// When `allow_hlt == true` CPU will treat HLT opcode as a legal opcode and function accordingly.
    pub fn set_allow_hlt(&mut self, mode: bool) {
        self.allow_hlt = mode;
    }

    /// Sets the `illegal_opcode_mode` field of the CPU.
    /// 
    /// When `illegal_opcode_mode == false` CPU will panic on encountering an illegal opcode 
    /// (except HLT if separately enabled).
    /// 
    /// When `illegal_opcode_mode == true` CPU will perform a NOP on encountering an illegal opcode
    /// (including HLT unless it's been separately enabled).
    pub fn set_illegal_opcode_mode(&mut self, mode: bool) {
        self.illegal_opcode_mode = mode;
    }

    /// Sets the `cycle_limit` field of the CPU.
    /// 
    /// CPU will halt before decoding next instruction if `cycle_count >= cycle_limit`.
    /// 
    /// Set `cycle_limit` to 0 to disable this behaviour.
    pub fn set_cycle_limit(&mut self, limit: usize) {
        self.cycle_limit = limit;
    }

    /// Set the contents of the CPU memory to `mem`
    pub fn flash_mem(&mut self, mem: Mem) {
        self.cpu_mem = mem;
    }

    /// Put the CPU in the standard power-on/reset state
    pub fn reset(&mut self) {
        self.pc = 0xFFFC;
        self.sp = 255;
        self.ac = 0;
        self.rx = 0;
        self.ry = 0;
        self.ps = BitField::new(0);
        self.cycles = 0;
    }
}