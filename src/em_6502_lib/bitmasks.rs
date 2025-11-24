use std::ops::{BitAnd, BitOr, BitXor};

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
        use BitMasks::*;
        match self {
            C => rhs & (0x01 << 0),
            Z => rhs & (0x01 << 1),
            I => rhs & (0x01 << 2),
            D => rhs & (0x01 << 3),
            B => rhs & (0x01 << 4),
            U => rhs & (0x01 << 5),
            V => rhs & (0x01 << 6),
            N => rhs & (0x01 << 7),
        }
    }
}

impl BitAnd<BitMasks> for u8 {
    type Output = u8;
    fn bitand(self, rhs: BitMasks) -> Self::Output {
        use BitMasks::*;
        match rhs {
            C => self & (0x01 << 0),
            Z => self & (0x01 << 1),
            I => self & (0x01 << 2),
            D => self & (0x01 << 3),
            B => self & (0x01 << 4),
            U => self & (0x01 << 5),
            V => self & (0x01 << 6),
            N => self & (0x01 << 7),
        }
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
        use BitMasks::*;
        match self {
            C => rhs | (0x01 << 0),
            Z => rhs | (0x01 << 1),
            I => rhs | (0x01 << 2),
            D => rhs | (0x01 << 3),
            B => rhs | (0x01 << 4),
            U => rhs | (0x01 << 5),
            V => rhs | (0x01 << 6),
            N => rhs | (0x01 << 7),
        }
    }
}

impl BitOr<BitMasks> for u8 {
    type Output = u8;
    fn bitor(self, rhs: BitMasks) -> Self::Output {
        use BitMasks::*;
        match rhs {
            C => self | (0x01 << 0),
            Z => self | (0x01 << 1),
            I => self | (0x01 << 2),
            D => self | (0x01 << 3),
            B => self | (0x01 << 4),
            U => self | (0x01 << 5),
            V => self | (0x01 << 6),
            N => self | (0x01 << 7),
        }
    }
}

impl BitXor<u8> for BitMasks {
    type Output = u8;
    fn bitxor(self, rhs: u8) -> Self::Output {
        use BitMasks::*;
        match self {
            C => rhs ^ (0x01 << 0),
            Z => rhs ^ (0x01 << 1),
            I => rhs ^ (0x01 << 2),
            D => rhs ^ (0x01 << 3),
            B => rhs ^ (0x01 << 4),
            U => rhs ^ (0x01 << 5),
            V => rhs ^ (0x01 << 6),
            N => rhs ^ (0x01 << 7),
        }
    }
}

impl BitXor<BitMasks> for u8 {
    type Output = u8;
    fn bitxor(self, rhs: BitMasks) -> Self::Output {
        use BitMasks::*;
        match rhs {
            C => self ^ (0x01 << 0),
            Z => self ^ (0x01 << 1),
            I => self ^ (0x01 << 2),
            D => self ^ (0x01 << 3),
            B => self ^ (0x01 << 4),
            U => self ^ (0x01 << 5),
            V => self ^ (0x01 << 6),
            N => self ^ (0x01 << 7),
        }
    }
}

impl From<BitMasks> for u8 {
    fn from(value: BitMasks) -> Self {
        use BitMasks::*;
        match value {
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