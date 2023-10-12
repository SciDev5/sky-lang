use std::vec;

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

pub trait ComplexConj {
    fn conj(self) -> Self;
}
impl ComplexConj for f64 {
    fn conj(self) -> Self {
        self
    }
}
impl ComplexConj for f32 {
    fn conj(self) -> Self {
        self
    }
}

struct MatrixRowIter<T, R: Iterator<Item = T>> {
    columns_per_row: usize,
    inner_iter: R,
}
impl<T, R: Iterator<Item = T>> Iterator for MatrixRowIter<T, R> {
    type Item = Vec<T>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut out = Vec::with_capacity(self.columns_per_row);
        for i in 0 .. self.columns_per_row {
            if let Some(next) = self.inner_iter.next() {
                out.push(next);
            } else {
                return None;
            }
        }
        Some(out)
    }
}


#[derive(Debug)]
pub struct Matrix<T> {
    shape: (usize, usize),
    data: Vec<T>,
}
impl<T> Matrix<T> {
    pub fn from_array<const W: usize, const H: usize>(array: [[T; W]; H]) -> Self {
        Self {
            shape: (W, H),
            data: array.into_iter().flat_map(|v| v.into_iter()).collect(),
        }
    }
    pub fn column<const H: usize>(array: [T; H]) -> Self {
        Self { shape: (1, H), data: array.into_iter().collect() }
    }
    pub fn row<const W: usize>(array: [T; W]) -> Self {
        Self { shape: (W, 1), data: array.into_iter().collect() }
    }
    pub fn to_tensor(self) -> Tensor<T> {
        Tensor {
            shape: vec![self.shape.0, self.shape.1],
            data: self.data,
        }
    }
    fn _data_index(shape: (usize, usize), real_index: (usize, usize)) -> usize {
        real_index.0 + shape.0 * real_index.1
    }
    pub fn into_iter_rows(&self) -> impl Iterator<Item = Vec<&T>> {
        MatrixRowIter { columns_per_row: self.shape.0, inner_iter: self.data.iter() }
    }
}
impl<T: ComplexConj> Matrix<T> {
    pub fn hermitian_conj(mut self) -> Self {
        self.hermitian_conj_self();
        self
    }
    pub fn hermitian_conj_self(&mut self) {
        let transposed_shape = (self.shape.1, self.shape.0);
        if self.shape.0 == 1 || self.shape.1 == 0 {
            self.shape = transposed_shape;
        } else if self.shape.0 == self.shape.1 {
            let size = self.shape.0;
            for i in 0..size - 1 {
                for j in i + 1..size {
                    self.data.swap(
                        Self::_data_index(self.shape, (i, j)),
                        Self::_data_index(transposed_shape, (i, j)),
                    );
                }
            }
        } else {
            todo!("hermitian conjugating oddly shaped matrices");
        }
    }
}

impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > ArrayAddSubDimChecked<Matrix<V>> for Matrix<T>
{
    type Output = Matrix<R>;
    fn add_checked_dims(self, rhs: Matrix<V>) -> Option<Self::Output> {
        if self.shape == rhs.shape {
            Some(Matrix {
                shape: self.shape,
                data: self
                    .data
                    .into_iter()
                    .zip(rhs.data.into_iter())
                    .map(|(a, b)| a + b)
                    .collect(),
            })
        } else {
            None
        }
    }
    fn sub_checked_dims(self, rhs: Matrix<V>) -> Option<Self::Output> {
        if self.shape == rhs.shape {
            Some(Matrix {
                shape: self.shape,
                data: self
                    .data
                    .into_iter()
                    .zip(rhs.data.into_iter())
                    .map(|(a, b)| a - b)
                    .collect(),
            })
        } else {
            None
        }
    }
    fn add_assign_checked_dims(&mut self, rhs: Matrix<V>) -> bool {
        if self.shape == rhs.shape {
            for (i, v) in rhs.data.into_iter().enumerate() {
                self.data[i] += v;
            }
            true
        } else {
            false
        }
    }
}
impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > std::ops::Add<Matrix<V>> for Matrix<T>
{
    type Output = Matrix<R>;
    fn add(self, rhs: Matrix<V>) -> Self::Output {
        self.add_checked_dims(rhs).unwrap_or_else(panic_dims_wrong)
    }
}
impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > std::ops::Sub<Matrix<V>> for Matrix<T>
{
    type Output = Matrix<R>;
    fn sub(self, rhs: Matrix<V>) -> Self::Output {
        self.sub_checked_dims(rhs).unwrap_or_else(panic_dims_wrong)
    }
}
impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > std::ops::AddAssign<Matrix<V>> for Matrix<T>
{
    fn add_assign(&mut self, rhs: Matrix<V>) {
        self.add_assign_unchecked(rhs)
    }
}

impl<T: std::ops::Mul<T, Output = T> + Copy> Matrix<T> {
    pub fn mul_factor(self, rhs: T) -> Self {
        Self {
            shape: self.shape,
            data: self.data.into_iter().map(|v| v * rhs).collect(),
        }
    }
}


impl<T: std::ops::Mul + std::iter::Sum + Copy> Matrix<T> {
    pub fn mul_matrix(self, rhs: Matrix<T>) -> Option<Self> {
        if self.shape.0 == rhs.shape.1 {
            // let mut out = Vec::with_capacity(self.shape.1 * rhs.shape.0);
            // let out_shape = (self.shape.0, rhs.shape.1);
            // for x in 0 ..
            todo!("matrix multiply")
        } else {
            None
        }
    }
}

impl<T: Clone> Clone for Matrix<T> {
    fn clone(&self) -> Self {
        Self {
            shape: self.shape,
            data: self.data.clone(),
        }
    }
    fn clone_from(&mut self, source: &Self) {
        self.shape.clone_from(&source.shape);
        self.data.clone_from(&source.data);
    }
}


#[cfg(test)]
mod test_mat {
    use super::Matrix;

    #[test]
    fn test() {
        let a = Matrix::column([1.0,2.0,1.0]);

        dbg!(a.clone().hermitian_conj());
    }
}




pub struct Tensor<T> {
    shape: Vec<usize>,
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
                .zip(real_index.iter())
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
    pub fn pointwise<U, R, F: Fn(T, U) -> R>(self, rhs: Tensor<U>, map: F) -> Option<Tensor<R>> {
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

    pub fn to_matrix(self) -> Option<Matrix<T>> {
        if self.rank() == 2 {
            Some(Matrix {
                shape: (self.shape[0], self.shape[1]),
                data: self.data,
            })
        } else {
            None
        }
    }
}

impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > ArrayAddSubDimChecked<Tensor<V>> for Tensor<T>
{
    type Output = Tensor<R>;
    fn add_checked_dims(self, rhs: Tensor<V>) -> Option<Self::Output> {
        if self.shape != rhs.shape {
            return None;
        }
        Some(Tensor::<R> {
            shape: self.shape,
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(a, b)| a + b)
                .collect(),
        })
    }
    fn sub_checked_dims(self, rhs: Tensor<V>) -> Option<Self::Output> {
        if self.shape != rhs.shape {
            return None;
        }
        Some(Tensor::<R> {
            shape: self.shape,
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(a, b)| a - b)
                .collect(),
        })
    }
    fn add_assign_checked_dims(&mut self, rhs: Tensor<V>) -> bool {
        if self.shape != rhs.shape {
            return false;
        }
        for (i, v) in rhs.data.into_iter().enumerate() {
            self.data[i] += v;
        }
        return true;
    }
}
impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > std::ops::Add<Tensor<V>> for Tensor<T>
{
    type Output = Tensor<R>;
    fn add(self, rhs: Tensor<V>) -> Self::Output {
        self.add_checked_dims(rhs).unwrap_or_else(panic_dims_wrong)
    }
}
impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > std::ops::Sub<Tensor<V>> for Tensor<T>
{
    type Output = Tensor<R>;
    fn sub(self, rhs: Tensor<V>) -> Self::Output {
        self.sub_checked_dims(rhs).unwrap_or_else(panic_dims_wrong)
    }
}
impl<
        T: std::ops::Add<V, Output = R> + std::ops::Sub<V, Output = R> + std::ops::AddAssign<V>,
        V,
        R,
    > std::ops::AddAssign<Tensor<V>> for Tensor<T>
{
    fn add_assign(&mut self, rhs: Tensor<V>) {
        self.add_assign_unchecked(rhs)
    }
}

impl<T: std::ops::Mul<T, Output = T> + Copy> Tensor<T> {
    pub fn mul_factor(self, rhs: T) -> Self {
        Self {
            shape: self.shape,
            data: self.data.into_iter().map(|v| v * rhs).collect(),
        }
    }
}

impl<T: Clone> Clone for Tensor<T> {
    fn clone(&self) -> Self {
        Self {
            shape: self.shape.clone(),
            data: self.data.clone(),
        }
    }
    fn clone_from(&mut self, source: &Self) {
        self.shape.clone_from(&source.shape);
        self.data.clone_from(&source.data);
    }
}

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
