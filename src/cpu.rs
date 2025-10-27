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
}

use std::num::Wrapping;
use cpu_ins::*;

pub type CPUByte = u8;
pub type CPUWord = u16;

pub const CPU_MEMSIZE: usize = 65536;

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
    /// Bit 0: Carry flag: The carry flag is set if the last operation caused an overflow from bit 7 of the result or an underflow from bit 0. This condition is set during arithmetic, comparison and during logical shifts. It can be explicitly set using the "Set Carry Flag" (SEC) instruction and cleared with "Clear Carry Flag" (CLC).
    /// Bit 1: Zero flag: The zero flag is set if the result of the last operation was zero.
    /// Bit 2: Interrupt disable: The interrupt disable flag is set if the program has executed a "Set Interrupt Disable" (SEI) instruction. While this flag is set the processor will not respond to interrupts from devices until it is cleared by a "Clear Interrupt Disable" (CLI) instruction.
    /// Bit 3: Decimal mode: While the decimal mode flag is set the processor will obey the rules of Binary Coded Decimal (BCD) arithmetic during addition and subtraction.
    /// Bit 4: Break command: The break command bit is set when a BRK instruction has been executed and an interrupt has been generated to process it.
    /// Bit 5: Overflow flag: Set during arithmetic operations if the result has yielded an invalid 2's complement result (e.g. adding two positive numbers nd ending up with a negative result). It is determined by looking at the carry between bits 6 and 7 and between bit 7 and the carry flag.
    /// Bit 6: Negative flag: Set if the result of the last operation had bit 7 set to one.
    ps: CPUByte,

    cycle_count: usize,
    cpu_mem: [CPUByte; CPU_MEMSIZE],
}

/// Runtime functions (generally use cycles)
impl CPU6502 {
    pub fn new() -> Self {
        CPU6502 { 
            pc: 0,
            sp: 0,
            ac: 0,
            rx: 0,
            ry: 0,
            ps: 0,
            cycle_count: 0,
            cpu_mem: [0; CPU_MEMSIZE]
        }
    }

    pub fn new_with_mem(mem: [CPUByte; CPU_MEMSIZE]) -> Self {
        CPU6502 {
            pc: 0,
            sp: 0,
            ac: 0,
            rx: 0,
            ry: 0,
            ps: 0,
            cycle_count: 0,
            cpu_mem: mem
        }
    }

    pub fn flash_mem(&mut self, mem: [CPUByte; CPU_MEMSIZE]) {
        self.cpu_mem = mem;
    }

    pub fn reset(&mut self) {
        self.pc = 0;
        self.sp = 0;
        self.ac = 0;
        self.rx = 0;
        self.ry = 0;
        self.ps = 0;
        self.cycle_count = 0;
    }

    /// 1 cycle
    pub fn get_next_byte(&mut self) -> CPUByte {
        self.cycle_count += 1;
        let ret = self.cpu_mem[self.pc as usize];
        self.pc += 1;
        ret
    }

    /// 2 cycles
    pub fn get_next_word(&mut self) -> CPUWord {
        self.cycle_count += 2;
        let low = self.get_next_byte();
        let high = self.get_next_byte();
        (high as u16) << 8 + low as u16
    }

    /// 1 cycle
    pub fn get_byte_zp(&mut self, addr: CPUByte) -> CPUByte {
        self.cycle_count += 1;
        self.cpu_mem[(0x0000 + addr as CPUWord) as usize]
    }

    /// 1 cycle
    pub fn get_byte_abs(&mut self, addr: CPUWord) -> CPUByte {
        self.cycle_count += 1;
        self.cpu_mem[addr as usize]
    }

    /// 2 cycles
    pub fn get_word_zp(&mut self, addr: CPUByte) -> CPUWord {
        let low = self.get_byte_zp(addr);
        let high = self.get_byte_zp(addr + 1);
        (high as u16) << 8 + low as u16
    }

    /// 2 cycles
    pub fn get_word_abs(&mut self, addr: CPUWord) -> CPUWord {
        let low = self.get_byte_abs(addr);
        let high = self.get_byte_abs(addr + 1);
        (high as u16) << 8 + low as u16
    }

    /// 1 cycle
    pub fn add_bytes_wrap(&mut self, a: CPUByte, b: CPUByte) -> CPUByte {
        self.cycle_count += 1;
        (Wrapping(a) + Wrapping(b)).0
    }

    /// 1 cycle
    pub fn add_words_wrap(&mut self, a: CPUWord, b: CPUWord) -> CPUWord {
        self.cycle_count += 1;
        (Wrapping(a) + Wrapping(b)).0
    }

    /// 1 cycle
    pub fn get_imm(&mut self) -> CPUByte {
        self.get_next_byte()
    }

    /// 2 cycles
    pub fn get_zpg(&mut self) -> CPUByte {
        let addr = self.get_next_byte();
        self.get_byte_zp(addr)
    }

    /// 3 cycles
    pub fn get_zpx(&mut self) -> CPUByte {
        let addr = self.get_next_byte();
        let addr = self.add_bytes_wrap(addr, self.rx);
        self.get_byte_zp(addr)
    }

    /// 3 cycles
    pub fn get_zpy(&mut self) -> CPUByte {
        let addr = self.get_next_byte();
        let addr = self.add_bytes_wrap(addr, self.ry);
        self.get_byte_zp(addr)
    }

    /// 3 cycles
    pub fn get_abs(&mut self) -> CPUByte {
        let addr = self.get_next_word();
        self.get_byte_abs(addr)
    }

    /// 3 ?+ 1 cycles
    pub fn get_abx(&mut self) -> CPUByte {
        let tmp_addr = self.get_next_word();
        let addr = tmp_addr + self.rx as CPUWord;

        if addr / 256 > tmp_addr / 256 {
            self.cycle_count += 1;
        }

        self.get_byte_abs(addr)
    }

    /// 3 ?+ 1 cycles
    pub fn get_aby(&mut self) -> CPUByte {
        let tmp_addr = self.get_next_word();
        let addr = tmp_addr + self.ry as CPUWord;

        if addr / 256 > tmp_addr / 256 {
            self.cycle_count += 1;
        }

        self.get_byte_abs(addr)
    }

    /// 5 cycles
    pub fn get_inx(&mut self) -> CPUByte {
        let tmp_addr = self.get_next_byte();
        let tmp_addr = self.add_bytes_wrap(tmp_addr, self.rx);
        let addr = self.get_word_zp(tmp_addr);
        self.get_byte_abs(addr)
    }

    /// 4 ?+ 1 cycles
    pub fn get_iny(&mut self) -> CPUByte {
        let tmp_addr = self.get_next_byte();
        let tmp_addr = self.get_word_zp(tmp_addr);
        let addr = tmp_addr + self.ry as CPUWord;
        
        if addr / 256 > tmp_addr / 256 {
            self.cycle_count += 1;
        }

        self.get_byte_abs(addr)
    }

    /// 3 cycles
    pub fn push_byte(&mut self, val: CPUByte) {
        self.cycle_count += 3;
        self.cpu_mem[(0x0100 + self.sp as u16) as usize] = val;
        // Don't need to check is stack overflows. Will
        // likely crash process.
        self.sp = self.sp.wrapping_add(1);
    }

    /// 4 cycles
    pub fn pull_byte(&mut self) -> CPUByte {
        self.cycle_count += 4;
        // Don't need to check if stack underflows. Will
        // likely crash process.
        self.sp = self.sp.wrapping_sub(1);
        
        self.cpu_mem[(0x0100 + self.sp as u16) as usize]
    }

    /// 3 cycles
    pub fn push_word(&mut self, val: CPUWord) {
        self.cycle_count -= 3;
        let [high, low] = val.to_be_bytes();

        self.push_byte(low);
        self.push_byte(high);
    }

    /// 4 cycles
    pub fn pull_word(&mut self) -> CPUWord {
        self.cycle_count -= 4;
        let high = self.pull_byte();
        let low = self.pull_byte();

        (high as CPUWord) << 8 + low as CPUWord
    }

    pub fn execute_next_ins(&mut self) {
        let ins = self.get_next_byte();
        match ins {
            ADC_IMM => {
                let init_pos = self.ac & 0b1000_0000 == 0;
                self.ac += self.get_imm() + if self.is_carry() {1} else {0};
                
                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_ZPG => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_zpg() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_ZPX => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_zpx() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_ABS => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_abs() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_ABX => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_abx() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_ABY => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_aby() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_INX => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_inx() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            ADC_INY => {
                let init_pos = self.ac & 0b1000_0000 == 0;

                self.ac += self.get_iny() + if self.is_carry() {1} else {0};

                if init_pos && self.ac & 0b1000_0000 != 0 {
                    self.set_overflow();
                    self.set_carry();
                } else {
                    self.unset_overflow();
                    self.unset_carry();
                }
                
                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }
            },
            AND_IMM => {
                self.ac &= self.get_imm();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_ZPG => {
                self.ac &= self.get_zpg();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_ZPX => {
                self.ac &= self.get_zpx();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_ABS => {
                self.ac &= self.get_abs();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_ABX => {
                self.ac &= self.get_abx();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_ABY => {
                self.ac &= self.get_aby();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_INX => {
                self.ac &= self.get_inx();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            AND_INY => {
                self.ac &= self.get_iny();

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            ASL_ACC => {
                if self.ac & 0b1000_0000 != 0 {
                    self.set_carry();
                } else {
                    self.unset_carry();
                }

                self.ac <<= 1;
                self.cycle_count += 1;

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            ASL_ZPG => {
                let addr = self.get_next_byte() as usize;

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_carry();
                } else {
                    self.unset_carry();
                }

                self.cpu_mem[addr] <<= 1;
                self.cycle_count += 3;

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            ASL_ZPX => {
                let addr = self.get_next_byte();
                let addr = self.add_bytes_wrap(addr, self.rx) as usize;

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_carry();
                } else {
                    self.unset_carry();
                }
                
                self.cpu_mem[addr] <<= 1;
                self.cycle_count += 3;

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            ASL_ABS => {
                let addr = self.get_next_word() as usize;

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_carry();
                } else {
                    self.unset_carry();
                }

                self.cpu_mem[addr] <<= 1;
                self.cycle_count += 3;

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            ASL_ABX => {
                let addr = self.get_next_word();
                let addr = self.add_words_wrap(addr, self.rx as CPUWord) as usize;

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_carry();
                } else {
                    self.unset_carry();
                }

                self.cpu_mem[addr] <<= 1;
                self.cycle_count += 3;

                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.cpu_mem[addr] & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
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
            BIT_ZPG => {
                let val = self.get_zpg();

                if self.ac & val == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                self.cycle_count += 1;

                if val & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if val & 0b0100_0000 != 0 {
                    self.set_overflow();
                } else {
                    self.unset_overflow();
                }
            },
            BIT_ABS => {
                let val = self.get_abs();

                if self.ac & val == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                self.cycle_count += 1;

                if val & 0b1000_0000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }

                if val & 0b0100_0000 != 0 {
                    self.set_overflow();
                } else {
                    self.unset_overflow();
                }
            },
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
            BRK_IMP => {
                self.push_word(self.pc);
                self.push_byte(self.ps);
                
                self.set_break_command();
                self.pc = 0xFFFE;
            },
            // !
            // ! todo: Continue instruction implementation
            // !
            LDA_IMM => {
                self.ac = self.get_imm();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_ZPG => {
                self.ac = self.get_zpg();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_ZPX => {
                self.ac = self.get_zpx();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_ABS => {
                self.ac = self.get_abs();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_ABX => {
                self.ac = self.get_abx();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_ABY => {
                self.ac = self.get_aby();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_INX => {
                self.ac = self.get_inx();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            LDA_INY => {
                self.ac = self.get_iny();
                
                if self.ac == 0 {
                    self.set_zero();
                } else {
                    self.unset_zero();
                }

                if self.ac & 0b10000000 != 0 {
                    self.set_negative();
                } else {
                    self.unset_negative();
                }
            },
            _ => (),
        }
    }
}

/// Utility functions (don't use cycles)
impl CPU6502 {
    pub fn is_carry(&self) -> bool {
        self.ps & 0b00000001 != 0
    }

    pub fn is_zero(&self) -> bool {
        self.ps & 0b00000010 != 0
    }

    pub fn is_interrupt_disable(&self) -> bool {
        self.ps & 0b00000100 != 0
    }

    pub fn is_decimal_mode(&self) -> bool {
        self.ps & 0b00001000 != 0
    }

    pub fn is_break_command(&self) -> bool {
        self.ps & 0b00010000 != 0
    }

    pub fn is_overflow(&self) -> bool {
        self.ps & 0b00100000 != 0
    }

    pub fn is_negative(&self) -> bool {
        self.ps & 0b01000000 != 0
    }

    pub fn reset_status_flags(&mut self) {
        self.ps = 0b00000000;
    }

    pub fn set_carry(&mut self) {
        self.ps |= 0b00000001;
    }

    pub fn set_zero(&mut self) {
        self.ps |= 0b00000010;
    }

    pub fn set_interrupt_disable(&mut self) {
        self.ps |= 0b00000100;
    }

    pub fn set_decimal_mode(&mut self) {
        self.ps |= 0b00001000;
    }

    pub fn set_break_command(&mut self) {
        self.ps |= 0b00010000;
    }

    pub fn set_overflow(&mut self) {
        self.ps |= 0b00100000;
    }

    pub fn set_negative(&mut self) {
        self.ps |= 0b01000000;
    }

    pub fn unset_carry(&mut self) {
        self.ps &= 0b11111110;
    }

    pub fn unset_zero(&mut self) {
        self.ps &= 0b11111101;
    }

    pub fn unset_interrupt_disable(&mut self) {
        self.ps &= 0b11111011;
    }

    pub fn unset_decimal_mode(&mut self) {
        self.ps &= 0b11110111;
    }

    pub fn unset_break_command(&mut self) {
        self.ps &= 0b11101111;
    }

    pub fn unset_overflow(&mut self) {
        self.ps &= 0b11011111;
    }

    pub fn unset_negative(&mut self) {
        self.ps &= 0b10111111;
    }
}