use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr};

/// 8-bit bitfield
#[derive(Copy, Clone)]
pub struct BitField(u8);

impl BitField {
    pub fn new(inner: u8) -> Self {
        Self(inner)
    }

    pub fn to_inner(self) -> u8 {
        self.0
    }

    pub fn test_bit(&self, bit: BitMasks) -> bool {
        self.0 & u8::from(bit) != 0
    }

    pub fn set_bit(&mut self, bit: BitMasks, val: bool) {
        match val {
            true => self.0 |= u8::from(bit),
            false => self.0 &= !u8::from(bit),
        }
    }
}

/// Enum to simplify manipulating CPU bitflags
#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub enum BitMasks {
    /// Carry flag mask
    C,
    /// Zero flag mask
    Z,
    /// Interrupt disable flag mask
    I,
    /// BDR mode flag mask
    D,
    /// Break command flag mask
    B,
    /// Unused flag mask
    U,
    /// Overflow flag mask
    V,
    /// Negative flag mask
    N,
}

impl Shl<BitMasks> for u8 {
    type Output = u8;
    fn shl(self, rhs: BitMasks) -> Self::Output {
        use BitMasks::*;
        self << match rhs {
            C => 0,
            Z => 1,
            I => 2,
            D => 3,
            B => 4,
            U => 5,
            V => 6,
            N => 7,
        }
    }
}

impl Shr<BitMasks> for u8 {
    type Output = u8;
    fn shr(self, rhs: BitMasks) -> Self::Output {
        use BitMasks::*;
        self >> match rhs {
            C => 0,
            Z => 1,
            I => 2,
            D => 3,
            B => 4,
            U => 5,
            V => 6,
            N => 7,
        }
    }
}

impl BitAnd for BitMasks {
    type Output = u8;
    fn bitand(self, rhs: Self) -> Self::Output {
        if self == rhs {
            self.into()
        } else {
            0
        }
    }
}

impl BitAnd<u8> for BitMasks {
    type Output = u8;
    fn bitand(self, rhs: u8) -> Self::Output {
        rhs & 0x1u8 << self
    }
}

impl BitAnd<BitMasks> for u8 {
    type Output = u8;
    fn bitand(self, rhs: BitMasks) -> Self::Output {
        self & 0x1u8 << rhs
    }
}

impl BitOr for BitMasks {
    type Output = u8;
    fn bitor(self, rhs: Self) -> Self::Output {
        if self == rhs {
            self.into()
        } else {
            u8::from(self) + u8::from(rhs)
        }
    }
}

impl BitOr<u8> for BitMasks {
    type Output = u8;
    fn bitor(self, rhs: u8) -> Self::Output {
        rhs | 0x1u8 << self
    }
}

impl BitOr<BitMasks> for u8 {
    type Output = u8;
    fn bitor(self, rhs: BitMasks) -> Self::Output {
        self | 0x1u8 << rhs
    }
}

impl BitXor<u8> for BitMasks {
    type Output = u8;
    fn bitxor(self, rhs: u8) -> Self::Output {
        rhs ^ 0x1u8 << self
    }
}

impl BitXor<BitMasks> for u8 {
    type Output = u8;
    fn bitxor(self, rhs: BitMasks) -> Self::Output {
        self ^ 0x1u8 << rhs
    }
}

impl From<BitMasks> for u8 {
    fn from(value: BitMasks) -> Self {
        1u8 << value
    }
}

impl From<u8> for BitMasks {
    fn from(value: u8) -> Self {
        use BitMasks::*;
        match value {
            0 => C,
            1 => Z,
            2 => I,
            3 => D,
            4 => B,
            5 => U,
            6 => V,
            7 => N,
            _ => panic!("Invalid bitmask index"),
        }
    }
}