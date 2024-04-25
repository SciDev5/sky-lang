use std::{marker::PhantomData, rc::Rc};

use crate::{
    build::module_tree::FullId,
    common::common_module::{CMFunction, CMStruct, CommonModule},
};

pub struct CodeBuf<'a, Token: GenericToken<'a, State>, State> {
    state: State,
    content: String,
    phantom: PhantomData<&'a Token>,
    just_saw_space_sensitive: bool,
}
impl<'a, Token: GenericToken<'a, State>, State> CodeBuf<'a, Token, State> {
    pub fn new(state: State) -> Self {
        Self {
            state,
            content: String::new(),
            phantom: PhantomData,
            just_saw_space_sensitive: false,
        }
    }
    pub fn write<T: Codegen<'a, Token, State>>(&mut self, obj: &T) {
        obj.gen_(self);
    }
    pub fn write_newline(&mut self) {
        if !self.content.is_empty() {
            self.write_raw("\n");
            self.just_saw_space_sensitive = false;
        }
    }
    pub fn write_tokens<const N: usize>(&mut self, tokens: [Token; N]) {
        for token in tokens {
            token.gen_(self);
        }
    }
    pub fn to_string(self) -> String {
        self.content
    }
    #[doc(hidden)]
    pub fn write_raw<T: ToStrRef>(&mut self, str: T) {
        self.content.push_str(str.str());
    }
    #[doc(hidden)]
    pub fn write_space_sensitive_update(&mut self, space_sensitive: bool) {
        if self.just_saw_space_sensitive && space_sensitive {
            self.content.push_str(" ");
        }
        self.just_saw_space_sensitive = space_sensitive;
    }
}

pub trait Codegen<'a, Token: GenericToken<'a, State>, State> {
    fn gen_(&self, buf: &mut CodeBuf<'a, Token, State>);
}

#[doc(hidden)]
pub trait ToStrRef {
    fn str(&self) -> &str;
}
impl<'a> ToStrRef for &'a str {
    fn str(&self) -> &str {
        *self
    }
}
impl ToStrRef for String {
    fn str(&self) -> &str {
        self.as_str()
    }
}

#[doc(hidden)]
pub trait GenericToken<'a, State>: Copy + Codegen<'a, Self, State> {}

#[macro_export]
macro_rules! define_codegen_token_enum {
    (
        $name:ident < $lt:lifetime , $state:ty >;
        $(
            $( #[ $k_meta:meta ] )*
            $k_name:ident $( ( $k_arg_name:ident : $k_arg_ty:ty ) )?
            $( [space_sensitive = $k_space_sensitive:expr] )?
            =>
            $($k_expr:expr);+
        ),* $(,)?
    ) => {
        type CodeBuf<'a> = $crate::codegen::common::CodeBuf<'a, $name <'a> , $state >;
        trait Codegen<'a> {
            fn gen(&self, buf: &mut CodeBuf<'a>);
        }
        impl<'a, C: Codegen<'a>> $crate::codegen::common::Codegen<'a, $name <'a> , $state > for C {
            fn gen_(&self, buf: &mut CodeBuf<'a>) {
                self.gen(buf);
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq)]
        enum $name <$lt> {
            $(
                $( #[ $k_meta ] )*
                $k_name $( ( $k_arg_ty ) )?
            ),*
        }
        impl<$lt> $crate::codegen::common::GenericToken<$lt , $state> for $name <$lt> {}
        impl<$lt> Codegen<$lt> for $name <$lt> {
            fn gen(&self, buf: &mut CodeBuf) {
                match self {
                    $(
                        Self:: $k_name $( ( $k_arg_name ) )?
                        => {
                            const SPACE_SENSITIVE: bool = $( $k_space_sensitive || )? false;
                            buf.write_space_sensitive_update(SPACE_SENSITIVE);
                            $(
                                buf.write_raw( $k_expr );
                            )+
                        }
                    )*
                }
            }
        }
    };
}

pub trait MDLTypes<'a> {
    type DataRef: Copy;
    type Function;
    type Struct;

    fn function_name(i: u32, f: &'a CMFunction) -> Self::DataRef;
    fn struct_name(i: u32, f: &'a CMStruct) -> Self::DataRef;
}
pub struct MergedDataList<'a, Types: MDLTypes<'a>> {
    source: &'a Vec<Rc<CommonModule>>,
    mod_zero_index_functions: Vec<usize>,
    mod_zero_index_structs: Vec<usize>,

    current_mod: usize,
    current_mod_deps: Vec<usize>,

    pub function_names: Vec<Types::DataRef>,
    functions: Vec<Types::Function>,
    pub struct_names: Vec<Types::DataRef>,
    structs: Vec<Types::Struct>,
}
impl<'a, Types: MDLTypes<'a>> MergedDataList<'a, Types> {
    pub fn new(source: &'a Vec<Rc<CommonModule>>) -> Self {
        let mut mod_zero_index_functions = Vec::with_capacity(source.len() + 1);
        let mut mod_zero_index_structs = Vec::with_capacity(source.len() + 1);

        let mut function_names = Vec::new();
        let mut struct_names = Vec::new();
        for cm in source {
            mod_zero_index_functions.push(function_names.len());
            mod_zero_index_structs.push(struct_names.len());

            function_names.extend(
                cm.functions
                    .iter()
                    .enumerate()
                    .map(|(i, f)| Types::function_name(i as u32, f)),
            );
            struct_names.extend(
                cm.structs
                    .iter()
                    .enumerate()
                    .map(|(i, f)| Types::struct_name(i as u32, f)),
            );
        }
        mod_zero_index_functions.push(function_names.len());
        mod_zero_index_structs.push(struct_names.len());

        let functions = Vec::with_capacity(function_names.len());
        let structs = Vec::with_capacity(struct_names.len());

        Self {
            source,
            mod_zero_index_functions,
            mod_zero_index_structs,
            current_mod: 0,
            current_mod_deps: Vec::new(),
            functions,
            structs,
            function_names,
            struct_names,
        }
    }
    pub fn iters(&self) -> MDLIters<'a, Types> {
        MDLIters {
            source: self.source[self.current_mod].as_ref(),
            zero_index_functions: self.mod_zero_index_functions[self.current_mod] as u32,
            zero_index_structs: self.mod_zero_index_structs[self.current_mod] as u32,
            _phantom: PhantomData,
        }
    }
    /// Pushes the module's function/struct/whatever code to this list
    /// and increments the module id.
    pub fn push_code(
        functions: impl Iterator<Item = Types::Function>,
        structs: impl Iterator<Item = Types::Struct>,
        self_: &mut Self,
    ) {
        debug_assert!(self_.current_mod < self_.source.len());

        self_.functions.extend(functions);
        self_.structs.extend(structs);

        self_.current_mod += 1;
        if self_.current_mod < self_.source.len() {
            self_.current_mod_deps = self_.source[self_.current_mod]
                .as_ref()
                .submodule_tree
                .get_dependency_list()
                .iter()
                .map(|it| it.id.0)
                .collect();

            debug_assert_eq!(
                self_.functions.len(),
                self_.mod_zero_index_functions[self_.current_mod]
            );
            debug_assert_eq!(
                self_.structs.len(),
                self_.mod_zero_index_structs[self_.current_mod]
            );
        }
    }
    pub fn as_destructure(&'a self) -> MDLDestructure<'a, Types> {
        let Self {
            functions, structs, ..
        } = &self;
        MDLDestructure { functions, structs }
    }

    pub fn lookup_function(&self, id: FullId) -> (usize, Types::DataRef) {
        let (mod_id, local_id) = match id {
            FullId::Local(id) => (self.current_mod, id),
            FullId::NonLocal { dependency_id, id } => (self.current_mod_deps[dependency_id], id),
        };
        let i = self.mod_zero_index_functions[mod_id] + local_id;
        (i, self.function_names[i])
    }
    pub fn lookup_struct(&self, id: FullId) -> (usize, Types::DataRef) {
        let (mod_id, local_id) = match id {
            FullId::Local(id) => (self.current_mod, id),
            FullId::NonLocal { dependency_id, id } => (self.current_mod_deps[dependency_id], id),
        };
        let i = self.mod_zero_index_structs[mod_id] + local_id;
        (i, self.struct_names[i])
    }
}
pub struct MDLIters<'a, Types: MDLTypes<'a>> {
    source: &'a CommonModule,
    zero_index_functions: u32,
    zero_index_structs: u32,
    _phantom: PhantomData<Types>,
}
impl<'a, Types: MDLTypes<'a>> MDLIters<'a, Types> {
    pub fn functions(&self) -> Vec<(u32, &'a CMFunction)> {
        self.source
            .functions
            .iter()
            .enumerate()
            .map(|(i, v)| (i as u32 + self.zero_index_functions, v))
            .collect()
    }
    pub fn structs(&self) -> Vec<(u32, &'a CMStruct)> {
        self.source
            .structs
            .iter()
            .enumerate()
            .map(|(i, v)| (i as u32 + self.zero_index_structs, v))
            .collect()
    }
}

pub struct MDLDestructure<'a, Types: MDLTypes<'a>> {
    pub functions: &'a Vec<Types::Function>,
    pub structs: &'a Vec<Types::Struct>,
}
