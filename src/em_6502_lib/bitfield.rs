use crate::bitmasks::BitMasks;

/// Struct representing a bitfield of 8 bits
#[derive(Copy, Clone)]
pub struct BitField(u8);

/// Struct to allow mutalbe bitwise interaction with BitField
#[derive(Copy, Clone)]
pub struct BitsMut([bool; 8]);

impl From<BitField> for BitsMut {
    fn from(value: BitField) -> Self {
        BitsMut([
            value.0 & 0b0000_0001 != 0,
            value.0 & 0b0000_0010 != 0,
            value.0 & 0b0000_0100 != 0,
            value.0 & 0b0000_1000 != 0,
            value.0 & 0b0001_0000 != 0,
            value.0 & 0b0010_0000 != 0,
            value.0 & 0b0100_0000 != 0,
            value.0 & 0b1000_0000 != 0,
        ])
    }
}

impl From<BitsMut> for BitField {
    fn from(value: BitsMut) -> Self {
        BitField(
            0 
            + if value.0[0] { 0x01 << 0 } else { 0 }
            + if value.0[1] { 0x01 << 1 } else { 0 }
            + if value.0[2] { 0x01 << 2 } else { 0 }
            + if value.0[3] { 0x01 << 3 } else { 0 }
            + if value.0[4] { 0x01 << 4 } else { 0 }
            + if value.0[5] { 0x01 << 5 } else { 0 }
            + if value.0[6] { 0x01 << 6 } else { 0 }
            + if value.0[7] { 0x01 << 7 } else { 0 }
        )
    }
}

impl BitField {
    pub fn new(v: u8) -> Self {
        Self(v)
    }

    pub fn to_inner(self) -> u8 {
        self.0
    }

    pub fn test_bit(&self, b: BitMasks) -> bool {
        self.0 & u8::from(b) != 0
    }

    pub fn set_bit(&mut self, b: BitMasks, v: bool) {
        let mut tmp = BitsMut::from(*self);
        tmp.0[u8::from(b) as usize] = v;
        *self = BitField::from(tmp);
    }
}