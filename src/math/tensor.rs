pub trait ArrayAddSubDimChecked<Rhs = Self> {
    type Output;
    fn add_checked_dims(self, rhs: Rhs) -> Option<Self::Output>;
    fn sub_checked_dims(self, rhs: Rhs) -> Option<Self::Output>;
    fn add_assign_checked_dims(&mut self, rhs: Rhs) -> bool;
    fn add_assign_unchecked(&mut self, rhs: Rhs) {
        if !self.add_assign_checked_dims(rhs) {
            panic_dims_wrong::<()>();
        }
    }
}
fn panic_dims_wrong<T>() -> T {
    panic!("attempt to add/subtract arrays with incompatible shape")
}

#[derive(Debug, Clone, Copy)]
pub enum OpSafetyLevel {
    Safe,
    SafeButNonsense,
    GuaranteedFailure,
}
impl OpSafetyLevel {
    pub fn is_safe_strict(self) -> bool {
        self.is_safe(false)
    }
    pub fn is_safe(self, tolerate_nonsense: bool) -> bool {
        match self {
            OpSafetyLevel::Safe => true,
            OpSafetyLevel::SafeButNonsense => tolerate_nonsense,
            OpSafetyLevel::GuaranteedFailure => false,
        }
    }
}

pub trait Linear:
    std::ops::Add<Output = Self>
    + std::ops::Sub<Output = Self>
    + std::ops::AddAssign
    + std::iter::Sum
    + std::ops::Mul<Output = Self>
    + Clone
    + Default
{
    fn hermitian_conj(self) -> Self;
    fn commutator(self, rhs: Self) -> Self;

    fn op_safety_add(&self, rhs: &Self) -> OpSafetyLevel;
    fn op_safety_mul(&self, rhs: &Self) -> OpSafetyLevel;
}
impl Linear for f64 {
    fn hermitian_conj(self) -> Self {
        self
    }
    fn commutator(self, _other: Self) -> Self {
        0.0
    }
    fn op_safety_add(&self, _rhs: &Self) -> OpSafetyLevel {
        OpSafetyLevel::Safe
    }
    fn op_safety_mul(&self, _rhs: &Self) -> OpSafetyLevel {
        OpSafetyLevel::Safe
    }
}

#[derive(Debug, Clone)]
pub struct Tensor<T> {
    /// Dimensionality and shape of this tensor.
    ///
    /// In matrices (rank 2), this is in the format (width, height).
    shape: Vec<usize>,
    /// In the data array, the data referenced by the first index is stored together, getting more
    /// spaced out for higher index ranks.
    data: Vec<T>,
}

impl<T> Tensor<T> {
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    pub fn shape(&self) -> &[usize] {
        &self.shape[..]
    }
    pub fn axis_range(&self, axis: usize) -> std::ops::Range<usize> {
        0..self.shape[axis]
    }
    fn _data_index(&self, real_index: &[usize]) -> Option<usize> {
        if real_index.len() != self.rank()
            || real_index
                .iter()
                .zip(self.shape.iter())
                .any(|(i, sz)| i >= sz)
        {
            return None;
        }
        let mut i = 0;
        let mut index_scale = 1;
        for dim_id in 0..self.rank() {
            i += real_index[dim_id] * index_scale;
            index_scale *= self.shape[dim_id];
        }
        Some(i)
    }

    pub fn map<R, F: Fn(T) -> R>(self, map: F) -> Tensor<R> {
        Tensor {
            shape: self.shape,
            data: self.data.into_iter().map(|v| map(v)).collect(),
        }
    }
    pub fn op_elementwise<U, R, F: Fn(T, U) -> R>(
        self,
        rhs: Tensor<U>,
        map: F,
    ) -> Option<Tensor<R>> {
        if self.shape != rhs.shape {
            return None;
        }
        Some(Tensor {
            shape: self.shape,
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(a, b)| map(a, b))
                .collect(),
        })
    }

    pub fn new_matrix<const W: usize, const H: usize>(arr: [[T; W]; H]) -> Self {
        Self {
            shape: vec![W, H],
            data: arr.into_iter().flat_map(|v| v.into_iter()).collect(),
        }
    }
    pub fn new_row_vec<const L: usize>(arr: [T; L]) -> Self {
        Self {
            shape: vec![L, 1],
            data: arr.into_iter().collect(),
        }
    }
    pub fn new_column_vec<const L: usize>(arr: [T; L]) -> Self {
        Self {
            shape: vec![1, L],
            data: arr.into_iter().collect(),
        }
    }
    pub fn new_row_vec_iter<I: Iterator<Item = T>>(iter: I) -> Self {
        let data: Vec<T> = iter.collect();
        Self {
            shape: vec![data.len(), 1],
            data,
        }
    }
    pub fn new_column_vec_iter<I: Iterator<Item = T>>(iter: I) -> Self {
        let data: Vec<T> = iter.collect();
        Self {
            shape: vec![1, data.len()],
            data,
        }
    }
}
impl<T: Clone + Default> Tensor<T> {
    pub fn transpose(mut self) -> Option<Self> {
        if self.rank() != 2 {
            None
        } else if self.shape[0] == self.shape[1] {
            let sz = self.shape[0];
            for i in 0..sz {
                for j in i + 1..sz {
                    self.data.swap(i * sz + j, j * sz + i);
                }
            }
            Some(self)
        } else {
            let width = self.shape[0];
            let height = self.shape[1];
            let mut data = vec![None; self.data.len()];
            self.shape[1] = width;
            self.shape[0] = height;
            for (i, ent) in self.data.into_iter().enumerate() {
                let x0 = i % width;
                let y0 = i / width;
                data[y0 + x0 * height] = Some(ent);
            }
            self.data = data.into_iter().map(|v| v.unwrap()).collect();
            Some(self)
        }
    }
    pub fn new_matrix_iter<I: Iterator<Item = T>>(
        iter: &mut I,
        width: usize,
        height: usize,
    ) -> Self {
        let data: Vec<T> = iter
            .chain(std::iter::repeat(Default::default()))
            .take(width * height)
            .collect();
        Self {
            shape: vec![width, height],
            data,
        }
    }
}
impl<T: Linear> Tensor<T> {
    pub fn hermitian_conj(self) -> Option<Self> {
        Some(self.transpose()?.map(|v| v.hermitian_conj()))
    }
}

impl<T: Linear> std::ops::Add for Tensor<T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            shape: self.shape,
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(a, b)| a + b)
                .collect(),
        }
    }
}
impl<T: Linear> std::ops::Sub for Tensor<T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            shape: self.shape,
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(a, b)| a - b)
                .collect(),
        }
    }
}
impl<T: Linear> std::ops::Mul for Tensor<T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        if self.rank() == 2 {
            // matrices and row/column vectors
            let (lhs_w, lhs_h) = (self.shape[0], self.shape[1]);
            let (rhs_w, rhs_h) = (rhs.shape[0], rhs.shape[1]);

            let (out_w, out_h) = (rhs_w, lhs_h);

            if lhs_w != rhs_h {
                panic!("Attempt to matrix multiply matrices with incompatible dimensions [{} * {}] and [{} * {}].", lhs_w, lhs_h, rhs_w, rhs_h);
            }
            let sum_len = lhs_w;

            /*
                [a, b] * [c; d] = [a*c + b*d]
                [a; b] * [c, d] = [a*c, a*d; b*c, b*d]
            */

            let mut output = Vec::with_capacity(out_w * out_h);

            for y in 0..out_h {
                for x in 0..out_w {
                    output.push(
                        (0..sum_len)
                            .map(|i| {
                                self.data[self._data_index(&[i, y]).expect("index out of bounds")]
                                    .clone()
                                    * rhs.data
                                        [rhs._data_index(&[x, i]).expect("index out of bounds")]
                                    .clone()
                            })
                            .sum(),
                    );
                }
            }

            Self {
                data: output,
                shape: vec![out_w, out_h],
            }
        } else {
            panic!("Cannot directly multiply two non-matrix (rank 2) tensors, try pointwise or einsum.");
        }
    }
}

impl<T: std::cmp::PartialEq> std::cmp::PartialEq for Tensor<T> {
    fn eq(&self, other: &Self) -> bool {
        self.shape == other.shape && self.data == other.data
    }
}
impl<T: std::cmp::Eq> std::cmp::Eq for Tensor<T> {}

impl<T> std::ops::Index<&[usize]> for Tensor<T> {
    type Output = T;
    fn index(&self, index: &[usize]) -> &Self::Output {
        let i = self._data_index(index).unwrap_or_else(|| {
            panic!(
                "index ({:?}) can't index into tensor with shape ({:?})",
                &index,
                &self.shape[..]
            );
        });
        &self.data[i]
    }
}
impl<T> std::ops::IndexMut<&[usize]> for Tensor<T> {
    fn index_mut(&mut self, index: &[usize]) -> &mut Self::Output {
        let i = self._data_index(index).unwrap_or_else(|| {
            panic!(
                "index ({:?}) can't index into tensor with shape ({:?})",
                &index,
                &self.shape[..]
            );
        });
        &mut self.data[i]
    }
}
impl<T: std::fmt::Display + std::fmt::Debug> std::fmt::Display for Tensor<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.rank() == 2 {
            let width = self.shape[0];
            let height = self.shape[1];
            writeln!(f, "Tensor({},{}) [", width, height)?;
            for y in 0..height {
                for x in 0..width {
                    write!(f, "\t{}", self[&[x, y]])?;
                }
                writeln!(f)?;
            }
            write!(f, "]")?;
            Ok(())
        } else {
            write!(f, "<TODO> {:?}", &self)
        }
    }
}

#[cfg(test)]

mod test {
    use super::Tensor;

    #[test]
    fn matmul() {
        let col3 = Tensor::new_column_vec_iter((1..=3).map(|v| v as f64));
        let row3 = Tensor::new_row_vec_iter((1..=3).map(|v| v as f64));
        let row2 = Tensor::new_row_vec_iter((1..=2).map(|v| v as f64));
        assert_eq!(&(col3.clone() * row2.clone()).shape[..], &[2, 3]); // col * row -> matrix, outer product
        assert_eq!(&(row3.clone() * col3.clone()).shape[..], &[1, 1]); // row * col -> scalar, inner product
    }

    #[test]
    fn indexing() {
        let mut mat = Tensor::new_matrix([[0.0; 2]; 3]); // 3 rows, 2 cols, 2x3

        assert_eq!(mat._data_index(&[0, 0]), Some(0));
        mat[&[0, 0]] = 4.0;
        assert_eq!(mat.data[0], 4.0);
        assert_eq!(mat._data_index(&[1, 0]), Some(1));
        assert_eq!(mat._data_index(&[2, 0]), None);
        assert_eq!(mat._data_index(&[0, 2]), Some(4));
        mat[&[0, 2]] = 8.0;
        assert_eq!(mat.data[4], 8.0);
        assert_eq!(mat._data_index(&[0, 3]), None);
        assert_eq!(mat._data_index(&[2, 3]), None);
        assert_eq!(mat._data_index(&[1, 2]), Some(5));
    }

    #[test]
    fn transpose_and_hc() {
        let mat2x3 = Tensor::new_matrix([[1.1, 1.2], [2.1, 2.2], [3.1, 3.2]]);
        let mat3x2 = mat2x3
            .clone()
            .transpose()
            .expect("should only be None if non-matrix");
        let mat3x2_hc = mat2x3
            .clone()
            .hermitian_conj()
            .expect("should only be None if non-matrix");

        assert_eq!(
            mat3x2,
            Tensor::new_matrix([[1.1, 2.1, 3.1], [1.2, 2.2, 3.2]]),
            "transposed matrix is what is expected"
        );
        assert_eq!(
            mat3x2_hc,
            Tensor::new_matrix([[1.1, 2.1, 3.1], [1.2, 2.2, 3.2]]),
            "hermitian conjugate matrix is what is expected"
        );

        assert_eq!(
            mat3x2
                .transpose()
                .expect("should only be None if non-matrix"),
            mat2x3,
            "testing double transposition results in no change."
        );
        assert_eq!(
            mat3x2_hc
                .hermitian_conj()
                .expect("should only be None if non-matrix"),
            mat2x3,
            "testing double hermitian conjugate results in no change."
        );
    }
}
