use crate::{mem::Mem, prelude::*};
use crate::{bitfield::BitField, bitmasks::BitMasks};

pub type InstructionResult = Result<CPUInstruction, String>;

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
    /// - Bit 5: Overflow flag: Set during arithmetic operations if the result has yielded an invalid
    /// 2's complement result (e.g. adding two positive numbers and ending up with a negative result).
    /// It is determined by looking at the carry between bits 6 and 7 and between bit 7 and the carry flag.
    /// - Bit 6: Negative flag: Set if the result of the last operation had bit 7 set to one.
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
    /// Add two CPUBytes, wrapping if necessary.
    /// 
    /// Does not affect processor status flags.
    pub fn add_bytes_wrap(&mut self, a: CPUByte, b: CPUByte) -> CPUByte {
        self.debug_imm("add_bytes_wrap".to_string());

        let ret = a.wrapping_add(b);
        self.cycles += 1;
        self.debug_ret_byte("add_bytes_wrap", ret);

        ret
    }

    /// 1 cycle
    /// 
    /// Add two CPUWords, wrapping if necessary.
    /// 
    /// Does not affect processor status flags
    pub fn add_words_wrap(&mut self, a: CPUWord, b: CPUWord) -> CPUWord {
        self.debug_imm("add_words_wrap".to_string());

        let ret = a.wrapping_add(b);
        self.cycles += 1;
        self.debug_ret_word("add_words_wrap", ret);

        ret
    }

    /// 1 cycle
    ///
    /// Use Immediate addressing mode to obtain argument for CPU instruction
    pub fn imm(&mut self) -> CPUByte {
        self.push_debug_msg("get_imm".to_string());
        
        let ret = self.fetch_next_byte();
        self.debug();
        self.debug_ret_byte("get_imm", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
    /// 
    /// Use Zero Page addressing mode to obtain argument for CPU instruction
    pub fn zpg(&mut self) -> CPUByte {
        self.push_debug_msg("get_zpg".to_string());
        
        let addr = self.fetch_next_byte();
        let ret = self.fetch_byte_at(addr as CPUWord);
        self.debug();
        self.debug_ret_byte("get_zpg", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    /// 
    /// Use Zero Page,X mode to obtain argument for CPU instruction
    pub fn zpx(&mut self) -> CPUByte {
        self.push_debug_msg("get_zpx".to_string());
        
        let addr = self.fetch_next_byte();
        let addr = self.add_bytes_wrap(addr, self.rx);
        let ret = self.fetch_byte_at(addr as CPUWord);
        self.debug();
        self.debug_ret_byte("get_zpx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    /// 
    /// Use Zero Page,Y addressing mode to obtain argument for CPU instruction
    pub fn zpy(&mut self) -> CPUByte {
        self.push_debug_msg("get_zpy".to_string());
        
        let addr = self.fetch_next_byte();
        let addr = self.add_bytes_wrap(addr, self.ry);
        let ret = self.fetch_byte_at(addr as CPUWord);
        self.debug();
        self.debug_ret_byte("get_zpy", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 cycles
    /// 
    /// Use Absolute addressing mode to obtain argument for CPU instruction
    pub fn abs(&mut self) -> CPUByte {
        self.push_debug_msg("get_abs".to_string());
        
        let addr = self.fetch_next_word();
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("get_abs", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 ?+ 1 cycles
    /// 
    /// Use Absolute,X addressing mode to obtain argument for CPU instruction
    pub fn abx(&mut self) -> CPUByte {
        self.push_debug_msg("get_abx".to_string());
        
        let tmp_addr = self.fetch_next_word();
        let addr = tmp_addr + self.rx as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("get_abx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 3 ?+ 1 cycles
    /// 
    /// Use Absolute,Y addressing mode to obtain argument for CPU instruction
    pub fn aby(&mut self) -> CPUByte {
        self.push_debug_msg("get_aby".to_string());
        
        let tmp_addr = self.fetch_next_word();
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("get_aby", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 5 cycles
    /// 
    /// Use Pre-Indexed Indirect addressing mode to obtain argument for CPU instruction
    pub fn idx(&mut self) -> CPUByte {
        self.push_debug_msg("get_inx".to_string());
        
        let tmp_addr = self.fetch_next_byte();
        let tmp_addr = self.add_bytes_wrap(tmp_addr, self.rx);
        let addr = self.fetch_word_at(tmp_addr as CPUWord);
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("get_inx", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 4 ?+ 1 cycles
    /// 
    /// Use Post-Indexed addressing mode to obtain argument for CPU instruction
    pub fn idy(&mut self) -> CPUByte {
        self.push_debug_msg("get_iny".to_string());
        
        let tmp_addr = self.fetch_next_byte();
        let tmp_addr = self.fetch_word_at(tmp_addr as CPUWord);
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycles += 1;
        }
        
        let ret = self.fetch_byte_at(addr);
        self.debug();
        self.debug_ret_byte("get_iny", ret);
        
        self.restore_debug_msg();
        ret
    }

    /// 2 cycles
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
        self.cycles += 1;
        self.debug_imm("inc sp".to_string());

        self.restore_debug_msg();
    }

    /// 3 cycles
    /// 
    /// Pulls (pops) the last-pushed CPUByte value from the stack at the memory location indicated by sp (- 1).
    /// 
    /// Increments sp.
    pub fn pull_byte(&mut self) -> CPUByte {
        // Don't need to check if stack underflows. Will likely crash process, this is correct behaviour.
        self.push_debug_msg("pull_byte".to_string());

        self.sp = self.sp.wrapping_add(1);
        self.cycles += 1;
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
        self.cycles -= 1;
        self.debug();

        self.restore_debug_msg();

        self.push_debug_msg(format!("push_word => store high ({val:#04x})"));
        self.push_byte(high);
        self.cycles -= 1;
        self.debug();

        self.restore_debug_msg();
    }

    /// 3 cycles
    /// 
    /// Pulls (pops) the two last-pushed CPUByte values from the stack starting 
    /// at the memory location indicated by sp (- 1).
    /// 
    /// Increments sp twice.
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
    /// Sets carry bit to boolean value passed.
    pub fn update_c_if(&mut self, cond: bool) {
        self.ps.set_bit(BitMasks::C, cond);
        self.debug_imm(format!("carry_if ({cond})"));
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

/// CPU instruction decoding and execution functions
impl CPU6502 {
    /// 1 cycle
    /// 
    /// Fetch the next CPUByte and decode it as the next CPU instruction.
    pub fn decode_next_ins(&mut self) -> InstructionResult {
        use CPUInstruction::*;
        use CPUAddrMode::*;

        self.push_debug_msg("decode_next_ins".to_string());

        let opcode = self.fetch_next_byte();
        self.restore_debug_msg();
        self.push_debug_msg(format!("decode_next_ins ({opcode:#04X})"));

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
            0xFF => {
                if self.allow_hlt {
                    HLT(IMP) 
                } else if self.illegal_opcode_mode {
                    NOP(IMP)
                } else {
                    return Err(format!("{opcode:#04X}"))
                }
            },
            _ => {
                if self.illegal_opcode_mode {
                    NOP(IMP)
                } else {
                    return Err(format!("{opcode:#04X}"))
                }
            },
        };

        self.debug();
        self.debug_ret_ins(ret);

        self.restore_debug_msg();
        Ok(ret)
    }

    /// Takes as many cycles as the instruction being executed.
    /// 
    /// Matches on the next CPUByte into its Instruction code, then executes
    /// the corresponding instruction.
    pub fn execute_next_ins(&mut self) -> InstructionResult {
        use CPUInstruction::*;
        use CPUAddrMode::*;

        self.push_debug_msg("execute_next_ins".to_string());

        let ins = self.decode_next_ins()?;

        match ins {
            ADC(mode) => self.adc(mode),
            AND(mode) => self.and(mode),
            ASL(mode) => self.asl(mode),
            BCC(REL) => {
                self.push_debug_msg("BCC".to_string());
                self.branch(!self.ps.test_bit(BitMasks::C));
                self.restore_debug_msg();
            },
            BCS(REL) => {
                self.push_debug_msg("BCS".to_string());
                self.branch(self.ps.test_bit(BitMasks::C));
                self.restore_debug_msg();
            },
            BEQ(REL) => {
                self.push_debug_msg("BEQ".to_string());
                self.branch(self.ps.test_bit(BitMasks::Z));
                self.restore_debug_msg();
            },
            BIT(mode) => self.bit(mode),
            BMI(REL) => {
                self.push_debug_msg("BMI".to_string());
                self.branch(self.ps.test_bit(BitMasks::N));
                self.restore_debug_msg();
            },
            BNE(REL) => {
                self.push_debug_msg("BNE".to_string());
                self.branch(!self.ps.test_bit(BitMasks::Z));
                self.restore_debug_msg();
            },
            BPL(REL) => {
                self.push_debug_msg("BPL".to_string());
                self.branch(!self.ps.test_bit(BitMasks::N));
                self.restore_debug_msg();
            },
            BRK(IMP) => self.brk(),
            // !
            // ! todo: Continue instruction implementation
            // !
            HLT(IMP) => { 
                self.debug_imm("HLT".to_string());
                self.cycles += 1;
            },
            LDA(mode) => self.lda(mode),
            NOP(IMP) => { 
                self.cycles += 1; 
                self.debug_imm("NOP".to_string());
            }
            _ => return Err(format!("Unimplemented instruction: {ins:?}")),
        }
        self.restore_debug_msg();

        Ok(ins)
    }
}

/// CPU instruction implementation functions
impl CPU6502 {
    /// 1 - 5 cycles
    /// 
    /// Implements functionality of the ADC Instruction.
    pub fn adc(&mut self, mode: CPUAddrMode) {
        use CPUAddrMode::*;

        self.push_debug_msg("ADC".to_string());

        let init_val = self.ac;
        let add_val = if self.ps.test_bit(BitMasks::C) {1} else {0} + match mode {
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
        
        self.update_v(init_val, add_val, self.ac);
        self.update_c_if(self.ps.test_bit(BitMasks::V));
        self.update_n(self.ac);
        self.update_z(self.ac);

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
        self.update_n(self.ac);

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
            ZPX => Some(self.fetch_next_byte() as CPUWord),
            ABS => Some(self.fetch_next_word()),
            ABX => Some(self.fetch_next_word()),
            _ => panic!("Invalid address mode for ASL"),
        };

        let byte = match addr {
            None => &mut self.ac,
            Some(a) => self.cpu_mem.mut_byte_at(a),
        };

        let orig_byte = byte.clone();
        *byte <<= 1;
        self.cycles += 1;
        let new_byte = byte.clone();
        
        self.update_c_if(orig_byte & 0b1000_0000 != 0);
        self.update_z(self.ac);
        self.update_n(new_byte);

        self.restore_debug_msg();
    }

    /// 1 - 4 cycles
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
        self.cycles += 1;
        self.debug();

        if cond {
            self.cycles += 1;
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

        self.push_debug_msg("push_pc".to_string());
        self.push_word(self.pc);
        self.restore_debug_msg();

        self.push_debug_msg("push_ps".to_string());
        self.push_byte(self.ps.to_inner());
        self.restore_debug_msg();
        
        self.ps.set_bit(BitMasks::B, true);

        self.push_debug_msg("jmp_to_loc_at_irq_vector".to_string());
        self.pc = self.fetch_word_at(0xFFFE);
        self.restore_debug_msg();

        self.debug();
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
        self.update_n(val);

        self.ps.set_bit(BitMasks::V, val & 0b0100_0000 != 0);

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

        self.debug_imm("ret -> ac".to_string());

        self.update_z(self.ac);
        self.update_n(self.ac);

        self.restore_debug_msg();
    }
}

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
        write!(f, "cycles: {}\npc: {:#06X}\nps (NVuBDIZC): {:08b}\nsp: {:#04X}\nac: {:#04X}\nrx: {:#04X}\nry: {:#04X}\n--------------------\nmem_byte[pc]: {:#04X}\nmem_word[pc]: {:#06X}\n--------------------",
            self.cycles,
            self.pc,
            self.ps.to_inner(),
            self.sp,
            self.ac,
            self.rx,
            self.ry,
            self.cpu_mem.byte_at(self.pc),
            ((self.cpu_mem.byte_at(self.pc.wrapping_add(1)) as CPUWord) << 8) + self.cpu_mem.byte_at(self.pc) as CPUWord,
        )
    }
}

impl std::fmt::Debug for CPU6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:?}", self, self.cpu_mem)
    }
}