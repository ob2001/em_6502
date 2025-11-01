use crate::{prelude::*, cpu::CPU6502, bitfield::BitField};

pub type InstructionResult = Result<CPUInstruction, String>;

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

    // !
    // ! todo: add debugging information where it is missing
    // !
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
            }
            BCS(REL) => {
                self.push_debug_msg("BCS".to_string());
                self.branch(self.ps.test_bit(BitMasks::C));
                self.restore_debug_msg();
            }
            BEQ(REL) => {
                self.push_debug_msg("BEQ".to_string());
                self.branch(self.ps.test_bit(BitMasks::Z));
                self.restore_debug_msg();
            }
            BIT(mode) => self.bit(mode),
            BMI(REL) => {
                self.push_debug_msg("BMI".to_string());
                self.branch(self.ps.test_bit(BitMasks::N));
                self.restore_debug_msg();
            }
            BNE(REL) => {
                self.push_debug_msg("BNE".to_string());
                self.branch(!self.ps.test_bit(BitMasks::Z));
                self.restore_debug_msg();
            }
            BPL(REL) => {
                self.push_debug_msg("BPL".to_string());
                self.branch(!self.ps.test_bit(BitMasks::N));
                self.restore_debug_msg();
            }
            BRK(IMP) => self.brk(),
            BVC(REL) => {
                self.push_debug_msg("BVC".to_string());
                self.branch(!self.ps.test_bit(BitMasks::V));
                self.restore_debug_msg();
            }
            BVS(REL) => {
                self.push_debug_msg("BVS".to_string());
                self.branch(self.ps.test_bit(BitMasks::V));
                self.restore_debug_msg();
            }
            CLC(IMP) => {
                self.push_debug_msg("CLC".to_string());
                self.ps.set_bit(BitMasks::C, false);
                self.cycles += 1;
                self.restore_debug_msg();
            }
            CLD(IMP) => {
                self.push_debug_msg("CLD".to_string());
                self.ps.set_bit(BitMasks::D, false);
                self.cycles += 1;
                self.restore_debug_msg();
            }
            CLI(IMP) => {
                self.push_debug_msg("CLI".to_string());
                self.ps.set_bit(BitMasks::I, false);
                self.cycles += 1;
                self.restore_debug_msg();
            }
            CLV(IMP) => {
                self.push_debug_msg("CLV".to_string());
                self.ps.set_bit(BitMasks::V, false);
                self.cycles += 1;
                self.restore_debug_msg();
            }
            CMP(mode) => self.cmp(mode),
            CPX(mode) => self.cpx(mode),
            CPY(mode) => self.cpy(mode),
            DEC(mode) => self.dec(mode),
            DEX(IMP) => {
                self.push_debug_msg("DEX".to_string());
                self.rx = self.rx.wrapping_sub(1);
                self.cycles += 1;

                self.ps.set_bit(BitMasks::Z, self.rx == 0);
                self.ps.set_bit(BitMasks::N, self.rx & 0b1000_0000 != 0);
                self.restore_debug_msg();
            }
            DEY(IMP) => {
                self.push_debug_msg("DEY".to_string());
                self.ry = self.ry.wrapping_sub(1);
                self.cycles += 1;

                self.ps.set_bit(BitMasks::Z, self.ry == 0);
                self.ps.set_bit(BitMasks::N, self.ry & 0b1000_0000 != 0);
                self.restore_debug_msg();
            }
            EOR(mode) => self.eor(mode),
            HLT(IMP) => { 
                self.debug_imm("HLT".to_string());
                self.cycles += 1;
            }
            INC(mode) => self.inc(mode),
            INX(IMP) => {
                self.push_debug_msg("INX".to_string());
                self.rx = self.rx.wrapping_add(1);
                self.cycles += 1;

                self.ps.set_bit(BitMasks::Z, self.rx == 0);
                self.ps.set_bit(BitMasks::Z, self.rx & 0b1000_0000 != 0);
                self.restore_debug_msg();
            }
            INY(IMP) => {
                self.push_debug_msg("INY".to_string());
                self.ry = self.ry.wrapping_add(1);
                self.cycles += 1;

                self.ps.set_bit(BitMasks::Z, self.ry == 0);
                self.ps.set_bit(BitMasks::Z, self.ry & 0b1000_0000 != 0);
                self.restore_debug_msg();
            }
            JMP(mode) => self.jmp(mode),
            JSR(ABS) => {
                self.push_debug_msg("JSR".to_string());
                
                let addr = self.fetch_next_word();
                self.cycles += 1;
                self.push_word(self.pc.wrapping_sub(1));

                self.pc = addr;

                self.restore_debug_msg();
            }
            LDA(mode) => self.lda(mode),
            LDX(mode) => self.ldx(mode),
            LDY(mode) => self.ldy(mode),
            LSR(mode) => self.lsr(mode),
            NOP(IMP) => { 
                self.cycles += 1; 
                self.debug_imm("NOP".to_string());
            }
            ORA(mode) => self.ora(mode),
            PHA(IMP) => {
                self.push_debug_msg("PHA".to_string());

                self.push_byte(self.ac);
                self.cycles += 1;

                self.restore_debug_msg();
            }
            PHP(IMP) => {
                self.push_debug_msg("PHP".to_string());

                self.push_byte(self.ps.to_inner());
                self.cycles += 1;

                self.restore_debug_msg();
            }
            PLA(IMP) => {
                self.push_debug_msg("PLA".to_string());

                self.ac = self.pull_byte();
                self.cycles += 1;
                
                self.restore_debug_msg();
            }
            PLP(IMP) => {
                self.push_debug_msg("PLP".to_string());

                self.ps = BitField::new(self.pull_byte());
                self.cycles += 1;

                self.restore_debug_msg();
            }
            ROL(mode) => self.rol(mode),
            ROR(mode) => self.ror(mode),
            RTI(IMP) => {
                self.push_debug_msg("RTI".to_string());

                self.ps = BitField::new(self.pull_byte());
                self.cycles -= 1;
                self.pc = self.pull_word();

                self.restore_debug_msg();
            }
            RTS(IMP) => {
                self.push_debug_msg("RTS".to_string());

                let addr = self.pull_word().wrapping_sub(1);
                self.pc = addr;
                self.fetch_next_byte(); // Fetch and discard next byte. Moves pc to where it needs to be.

                self.restore_debug_msg();
            }
            SBC(mode) => self.sbc(mode),
            SEC(IMP) => {
                self.push_debug_msg("SEC".to_string());

                self.ps.set_bit(BitMasks::C, true);
                self.cycles += 1;

                self.restore_debug_msg();
            }
            SED(IMP) => {
                self.push_debug_msg("SED".to_string());

                self.ps.set_bit(BitMasks::D, true);
                self.cycles += 1;

                self.restore_debug_msg();
            }
            SEI(IMP) => {
                self.push_debug_msg("SEI".to_string());

                self.ps.set_bit(BitMasks::I, true);
                self.cycles += 1;

                self.restore_debug_msg();
            }
            STA(mode) => self.sta(mode),
            STX(mode) => self.stx(mode),
            STY(mode) => self.sty(mode),
            TAX(IMP) => {
                self.push_debug_msg("TAX".to_string());

                self.rx = self.ac;
                self.cycles += 1;

                self.ps.set_bit(BitMasks::N, self.rx & 0b1000_0000 != 0);
                self.ps.set_bit(BitMasks::Z, self.rx == 0);

                self.restore_debug_msg();
            }
            TAY(IMP) => {
                self.push_debug_msg("TAY".to_string());

                self.ry = self.ac;
                self.cycles += 1;

                self.ps.set_bit(BitMasks::N, self.ry & 0b1000_0000 != 0);
                self.ps.set_bit(BitMasks::Z, self.ry == 0);

                self.restore_debug_msg();
            }
            TSX(IMP) => {
                self.push_debug_msg("TSX".to_string());

                self.rx = self.sp;
                self.cycles += 1;

                self.ps.set_bit(BitMasks::N, self.rx & 0b1000_0000 != 0);
                self.ps.set_bit(BitMasks::Z, self.rx == 0);

                self.restore_debug_msg();
            }
            TXA(IMP) => {
                self.push_debug_msg("TXA".to_string());

                self.ac = self.rx;
                self.cycles += 1;

                self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);
                self.ps.set_bit(BitMasks::Z, self.ac == 0);

                self.restore_debug_msg();
            }
            TXS(IMP) => {
                self.push_debug_msg("TXS".to_string());

                self.sp = self.rx;
                self.cycles += 1;

                self.restore_debug_msg();
            }
            TYA(IMP) => {
                self.push_debug_msg("TYA".to_string());

                self.ac = self.ry;
                self.cycles += 1;

                self.ps.set_bit(BitMasks::N, self.ac & 0b1000_0000 != 0);
                self.ps.set_bit(BitMasks::Z, self.ac == 0);

                self.restore_debug_msg();
            }
            _ => return Err(format!("Unimplemented instruction: {ins:?}")),
        }
        self.restore_debug_msg();

        Ok(ins)
    }
}

