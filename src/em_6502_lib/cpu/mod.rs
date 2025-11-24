use crate::{prelude::*, mem::Mem, bitfield::BitField};

pub mod debug;
pub mod dec_exec;
pub mod ins;
pub mod runtime;

/// 56 spec instructions, +1 added instruction (HLT) for debugging
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum CPUInstruction {
    /// 2 - 6 cycles
    /// 
    /// This instruction adds the contents of a memory location to the accumulator
    /// together with the carry bit. If overflow occurs the carry bit is set, this enables
    /// multiple byte addition to be performed.
    ADC(CPUAddrMode),
    /// 2 - 6 cycles
    /// 
    /// A logical AND is performed, bit by bit, on the accumulator contents using the
    /// contents of a byte of memory.
    AND(CPUAddrMode),
    /// 2 - 7 cycles
    /// 
    /// This operation shifts all the bits of the accumulator or memory contents one
    /// bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag. The effect
    /// of this operation is to multiply the memory contents by 2 (ignoring 2's complement
    /// considerations), setting the carry if the result will not fit in 8 bits.
    ASL(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the carry flag is clear then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BCC(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the carry flag is set then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BCS(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the zero flag is set then add the relative displacement to the program
    /// counter to cause a brnch to a new location.
    BEQ(CPUAddrMode),
    /// 3 - 4 cycles
    /// 
    /// This instruction is used to test if one or more bits are set in a target memory
    /// location. The mask pattern in Acc is ANDed with the value in memory to set or
    /// clear the zero flag, but the result is not kept. Bits 7 and 6 of the value from
    /// memory are copied into the N and V flags.
    BIT(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the negative flag is set then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BMI(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the zero flag is clear then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BNE(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the negative flag is clear then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BPL(CPUAddrMode),
    /// 7 cycles
    /// 
    /// The BRK instruction forces the generation of an interrupt request. The program
    /// counter and processor status are pushed on the stack then the IRQ vector at $FFFE/F
    /// is loaded into the PC and the break flag in the status is set to one.
    BRK(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the overflow flag is clear then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BVC(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// If the overflow location is set then add the relative displacement to the program
    /// counter to cause a branch to a new location.
    BVS(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Set the carry flag to zero.
    CLC(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Set the decimal mode flag to zero.
    CLD(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Clears the interrupt disable flag allowing normal interrupt requests to be serviced.
    CLI(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Clears the overflow flag.
    CLV(CPUAddrMode),
    /// 2 - 6 cycles
    /// 
    /// This instruction compares the contents of the accumulator with another memory held
    /// value and sets the zero and carry flags as appropriate.
    CMP(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// This instruction compares the contents of the X register with another memory held
    /// value and sets the zero and carry flags as appropriate.
    CPX(CPUAddrMode),
    /// 2 - 4 cycles
    /// 
    /// This instruction compares the contents of the Y register with another memory held
    /// value and sets the zero and carry flags as appropriate.
    CPY(CPUAddrMode),
    /// 5 - 7 cycles
    /// 
    /// Subtracts one from the value held at a specified memory location setting the zero
    /// and negative flags as appropriate.
    DEC(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Subtracts one from the X register setting the zero and negative flags as appropriate.
    DEX(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Subtracts one from the Y register setting the zero and negative flags as appropriate.
    DEY(CPUAddrMode),
    /// 2 - 6 cycles
    /// 
    /// An exclusive OR is performed, bit by bit, on the accumulator contents using the
    /// contents of a  byte of memory.
    EOR(CPUAddrMode),

    /// Not part of CPU spec. Added to gracefully halt emulation.
    HLT(CPUAddrMode),

    /// 5 - 7 cycles
    /// 
    /// Adds one to the value held at a specified memory location setting the zero and negative
    /// flags as appropriate.
    INC(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Adds one to the X register setting the zero and negative flags as appropriate.
    INX(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Adds one to the Y register setting the zero and negative flags as appropriate.
    INY(CPUAddrMode),
    /// 3 or 5 cycles
    /// 
    /// Sets the program counter to the address specified by the operand.
    JMP(CPUAddrMode),
    /// 6 cycles
    /// 
    /// The JSR instruction pushes the address (minus one) of the return point on to the stack
    /// and then sets the program counter to the target memory address.
    JSR(CPUAddrMode),
    /// 2 - 6 cycles
    /// 
    /// Loads a byte of memory into the accumulator setting the zero and negative
    /// flags as appropriate.
    LDA(CPUAddrMode),
    /// 2 - 5 cycles
    /// 
    /// Loads a byte of memory into the X register setting the zero and negative
    /// flags as appropriate.
    LDX(CPUAddrMode),
    /// 2 - 5 cycles
    /// 
    /// Loads a byte of memory into the Y register setting the zero and negative
    /// flags as appropriate.
    LDY(CPUAddrMode),
    /// 2 - 7 cycles
    /// 
    /// Each of the bits in A or M is shifted one place to the right. The bit that was in
    /// bit 0 is shifted into the carry flag. Bit 7 is set to zero.
    LSR(CPUAddrMode),
    /// 2 cycles
    /// 
    /// The NOP instruction causes no changes to the processor other than the normal incrementing
    /// of the program counter to the next instruction.
    NOP(CPUAddrMode),
    /// 2 - 6 cycles
    /// 
    /// An inclusive OR is performed, bit by bit, on the accumulator contents using the contents
    /// of a byte of memory.
    ORA(CPUAddrMode),
    /// 3 cycles
    /// 
    /// Pushes a copy of the accumulator on to the stack.
    PHA(CPUAddrMode),
    /// 3 cycles
    /// 
    /// Pushes a copy of the status flags on to the stack.
    PHP(CPUAddrMode),
    /// 4 cycles
    /// 
    /// Pulls an 8 bit vale from the stack and into the accumulator. The zero and negative flags
    /// are set as appropriate.
    PLA(CPUAddrMode),
    /// 4 cycles
    /// 
    /// Pulls an 8 bit value from the stack and into the processor flags. The flags will take on
    /// new states as determined by the value pulled.
    PLP(CPUAddrMode),
    /// 2 - 7 cycles
    /// 
    /// Move each of the bits in either Acc or Memory location one place to the left. Bit 0 is filled with
    /// the current value of the carry flag whilst the old bit 7 becomes the new carry flag value.
    ROL(CPUAddrMode),
    /// 2 - 7 cycles
    /// 
    /// Move each of the bits in either Acc or Memory location one place to the right. Bit 7 is filled with
    /// the current value of the carry flag whilst the old bit 0 becomes the new carry flag value.
    ROR(CPUAddrMode),
    /// 6 cycles
    /// 
    /// The RTI instruction is used at the end of an interrupt processing routine. It pulls the processor
    /// flags from the stack followed by the program counter.
    RTI(CPUAddrMode),
    /// 6 cycles
    /// 
    /// The RTS instruction is used at the end of a subroutine to return to the calling routine. It pulls
    /// the program counter (minus one) from the stack.
    RTS(CPUAddrMode),
    /// 2 - 6 cycles
    /// 
    /// This instruction subtracts the contents of a memory location to the accumulator together with the not
    /// of the carry bit. If overflow occurs the carry bit is clear, this enables multiple byte subtraction to
    /// be performed.
    SBC(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Set the carry flag to one.
    SEC(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Set the decimal mode flag to one.
    SED(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Set the interrupt disable flag to one.
    SEI(CPUAddrMode),
    /// 3 - 6 cycles
    /// 
    /// Stores the contents of the accumulator into memory.
    STA(CPUAddrMode),
    /// 3 - 4 cycles
    /// 
    /// Stores the contents of the X register into memory.
    STX(CPUAddrMode),
    /// 3 - 4 cycles
    /// 
    /// Stores the contents of the Y register into memory.
    STY(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Copies the current contents of the accumulator into the X register and sets the
    /// zero and negative flags as appropriate.
    TAX(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Copies the current contents of the accumulator into the Y register and sets the zero
    /// and negative flags as appropriate.
    TAY(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Copies the current contents of the stack register into the X register and sets the zero
    /// and negative flags as appropriate.
    TSX(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Copies the current contents of the X register into the accumulator and sets the zero
    /// and negative flags as appropriate.
    TXA(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Copies the current contents of the X register into the stack register.
    TXS(CPUAddrMode),
    /// 2 cycles
    /// 
    /// Copies the current contents of the Y register into the accumulator and sets the zero
    /// and negative flags as appropriate.
    TYA(CPUAddrMode),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum CPUAddrMode {
    /// (1 or 0?) cycles
    /// 
    /// # Implicit Addressing Mode
    /// 
    /// For many 6502 instructions the source and destination of the information to be manipulated
    /// is implied directl by the function of the instruction itself and no further oprerand needs
    /// to be specified. Operations like "Clear Carry Flag" (CLC) and "Return from Subroutine" (RTS)
    /// are implicit.
    IMP,
    /// (1 or 0?) cycles
    /// 
    /// # Accumulator Addressing Mode
    /// 
    /// Some instructions have an option to operate directly upon the accumulator. The programmer
    /// specifies this by using a special operand value, "A".
    ACC,
    /// 1 cycle
    /// 
    /// # Immediate Addressing Mode
    /// 
    /// Immediate addressing allows the programmer to directly specify an 8 bit constant within the
    /// instruction. It is indicated by a "#" symbol followed by a numeric experession.
    IMM,
    /// 2 cycles
    /// 
    /// # Zero Page Addressing Mode
    /// 
    /// An instruction using zero page addressing mode has only an 8 bit address operand. This limits
    /// it to addressing only the first 256 bytes of memory (e.g. $0000 to $00FF) where the most
    /// significant byte of the address is always zero. In zero page mode only the least significant
    /// byte of the address is held in the instruction making it shorter by one byte (important for
    /// space saving) and one less memory fetch during execution (important for speed).
    /// 
    /// An assembler will automatically select zero page addressing mode if the operand evaluates to a
    /// zero page address and the instruction supports the mode (not all do).
    ZPG,
    /// 3 cycles
    /// 
    /// # Zero Page,X Addressing Mode
    /// 
    /// The address to be accessed by an instruction using indexed zero apge addressing is calculated by
    /// taking the 8 bit zero page address from the instruction and adding the current calue of the X
    /// register to it. For example if the X register containts $0F and the instruction LDA $80,X is
    /// executed then the accumulator will be loaded from $008F (e.g. $80 + $0F => $8F).
    /// 
    /// __NB__: The address calculation wraps around if the sum of the base address and the register exceed
    /// $FF. If we repeat the last example but with $FF in the X register then the accumulator will be
    /// loaded from $007F (e.g. $80 + $FF => $7F) and not $017F.
    ZPX,
    /// 3 cycles
    /// 
    /// # Zero Page,Y Addressing Mode
    /// 
    /// The address to be accessed by an instruction using indexed zero page addressing is calculated by
    /// taking the 8 bit zero page address from the instruction and adding the current value of the Y register
    /// to it. This mode can only be used with the LDX and STX instructions.
    ZPY,
    /// (1?) cycle
    /// 
    /// # Relative Addressing Mode
    /// 
    /// Relative addressing mode is used by branch instructions (e.g. BEQ, BNE, etc.) which contain a signed
    /// 8 bit relative offset (e.g. -128 to +127) which is added to program counter if the condition is true.
    /// As the program counter itself is incremented during instruction execution by two the effective address
    /// range for the target must be within -126 to +129 bytes of the branch.
    REL,
    /// 3 cycles
    /// 
    /// # Absolute Addressing Mode
    /// 
    /// Instructions using absolute addressing contain a full 16 bit address to identift the target location.
    ABS,
    /// 3 ?+ 1 cycles
    /// 
    /// # Absolute,X Addressing Mode
    /// 
    /// The address to be accessed by an instruction usin X register indexed absolute addressing is computed by
    /// taking the 16 bit address from the instruction and adding the contents of the X register. For example if
    /// X containts $92 then an STA $2000,X instruction will store the accumulator at $2092.
    ABX,
    /// 3 ?+ 1 cycles
    /// 
    /// # Absolute,Y Addressing Mode
    /// 
    /// The Y register indexed absolute addressing mode is the same as the previous mode only with the contents
    /// of the Y register added to the 16 bit address from the instruction.
    ABY,
    /// (?) cycles
    /// 
    /// # Indirect Addressing Mode
    /// 
    /// JMP is the only 6502 instruction to support indirection. The instruction contains a 16 bit address which
    /// identifies the location of the least significant bute of another 16 bit memory address which is the real
    /// target of the instruction.
    /// 
    /// For example if location $0120 contains $FC and location $0121 containts $BA then the instruction JMP ($0120)
    /// will cause the next instruction execution to occur at $BAFC (e.g. the contents of $0120 and $0121).
    IND,
    /// 5 cycles
    /// 
    /// # Indexed Indirect Addressing Mode
    /// 
    /// Indexed indirect addressing is normally used in conjunction with a table of addresses held in the zero page.
    /// The address of the table is taken from the instruction and the X register added to it (with zero page wrap
    /// around) to give the location of the least significant byte of the target address.
    IDX,
    /// 4 ?+ 1 cycles
    /// 
    /// # Indirect Indexed Addressing Mode
    /// 
    /// Indirect indexed addressing is the most common indirection mode used in the 6502. An instruction contains the
    /// zero page location of the least significant byte of a 16 bit address. The Y register is dynamically added to
    /// this value to generate the actual target address for operation.
    IDY,
}

pub struct CPU6502 {
    /// Internal
    /// 
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

    /// Internal
    /// 
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

    /// Internal
    /// 
    /// Accumulator
    /// 
    /// The 8 bit accumulator is used in all arithmetic and logical operations (with the
    /// exception of increments and decrements). The contents of the accumulator can be
    /// stored and retrieved either from memory or the stack.
    /// 
    /// Most complex operations will need to use the accumulator for arithmetic and
    /// efficient optimisation of its use is a key feature of time critical routines.
    ac: CPUByte,

    /// Internal
    /// 
    /// Index register X
    /// 
    /// The 8 bit index register is most commonly used to hold counters or offsets for
    /// accessing memory. The value of the X register can be loaded and saved in memory,
    /// compared with values held in memory, or incremented and decremented.
    /// 
    /// The X register has one special function. It can be used to get a copy of the stack
    /// pointer or change its value.
    rx: CPUByte,

    /// Internal
    /// 
    /// Index register Y
    /// 
    /// The Y register is similar to the X register in that it is available for holding counter
    /// or offsets memory access and supports the same set of memory load, save, and compare
    /// operations as well as increments and decrements. It has no special functions.
    ry: CPUByte,

    /// Internal
    /// 
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

    /// Internal
    /// 
    /// Counter incremented to emulate CPU clock cycles
    cycles: usize,

    /// Contains all memory the CPU can access, ordered as CPU expects:
    /// - Page 0 (0x0000 - 0x00FF): Zero page memory
    /// - Page 1 (0x0100 - 0x01FF): Stack memory
    /// - 0xFFFA/0xFFFB: Location of non-maskable interrupt handler
    /// - 0xFFFC/0xFFFD: Location of power on/reset location
    /// - 0xFFFE/0xFFFF: Location of BRK/interrupt request handler
    cpu_mem: Mem,

    /// Flag to disable or enbable debug output during execution
    dbg: bool,

    /// Internal
    /// 
    /// Used for debug message formatting to track processor execution state
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

/// CPU creation/setup functions. These do not interact with runtime (No CPU cycles)
impl CPU6502 {
    /// Create a new CPU6502 with registers set to defaults and zeroed memory
    pub fn default() -> Self {
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

    /// Create a new CPU6502 with memory `mem`
    pub fn new_with_mem(mem: Mem) -> Self {
        let mut cpu = Self::default();
        cpu.flash_mem(mem);
        cpu
    }

    /// Create a new CPU6502 with memory as specified in specification file `mem_file`
    pub fn new_with_mem_from_file(mem_file: String) -> Result<Self, String> {
        if let Ok(mem) = Mem::new_from_file(mem_file.clone()) {
            Ok(Self::new_with_mem(mem))
        } else {
            Err(format!("Error loading or parsing memory file: {mem_file}"))
        }
    }

    /// Set the `allow_hlt` field of the CPU.
    /// 
    /// When `allow_hlt == false` CPU will treat HLT opcode (0xFF) as any other illegal opcode.
    /// 
    /// When `allow_hlt == true` CPU will treat HLT opcode as a legal opcode and function accordingly.
    pub fn set_allow_hlt(&mut self, mode: bool) {
        self.allow_hlt = mode;
    }

    /// Set the `illegal_opcode_mode` field of the CPU.
    /// 
    /// When `illegal_opcode_mode == false` CPU will panic on encountering an illegal opcode 
    /// (except HLT if separately enabled).
    /// 
    /// When `illegal_opcode_mode == true` CPU will perform a NOP on encountering an illegal opcode
    /// (including HLT unless it's been separately enabled).
    pub fn set_illegal_opcode_mode(&mut self, mode: bool) {
        self.illegal_opcode_mode = mode;
    }

    /// Set the `cycle_limit` field of the CPU.
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