use std::{marker::PhantomData, path::PathBuf, rc::Rc, str::FromStr};

use crate::{
    common::{
        backend::{BackendCompiler, BackendId, CommonBackend, PlatformInfo},
        common_module::{CMExpression, CMFunction, CMLiteralValue, CMStruct, CMType, CommonModule},
    },
    define_codegen_token_enum,
    parse::macros::MacroObject,
};

use super::common::{MDLDestructure, MDLTypes, MergedDataList};

pub struct CPPCodegenBackend;
impl BackendCompiler for CPPCodegenBackend {
    const ID: BackendId = 0x0000_6626_86d7_a1fa;
    const PLATFORM_INFO: PlatformInfo = PlatformInfo {
        id: Self::ID,
        name: "codegen_cpp",
        compat_ids: &[CommonBackend::ID],
    };
    type Config = (PathBuf,);
    type Output = (String, String);
    fn compile(&self, source: &Vec<Rc<CommonModule>>, config: (PathBuf,)) -> Self::Output {
        let mut list = MergedDataList::new(source);

        for (i, cm) in source.iter().map(Rc::as_ref).enumerate() {
            let iters = list.iters();
            MergedDataList::push_code(
                iters
                    .functions()
                    .into_iter()
                    .map(|(id, f)| convert_function(id, f, &list))
                    .collect::<Vec<_>>()
                    .into_iter(),
                iters
                    .structs()
                    .into_iter()
                    .map(|_| todo!("// TODO c++ codegen translate structs"))
                    .collect::<Vec<_>>()
                    .into_iter(),
                &mut list,
            );
        }

        let MDLDestructure { functions, structs } = list.as_destructure();

        let file_name = config.0;
        let headers: Vec<CPPHeader> = Vec::new();

        let mut buf_content = CodeBuf::new(CPPCodegenState {
            list: &list,
            current_locals: &[],
        });
        let mut buf_header = CodeBuf::new(CPPCodegenState {
            list: &list,
            current_locals: &[],
        });

        {
            for header in &headers {
                buf_header.write(header);
            }
            buf_content.write(&CPPHeader::LocalOwned({
                let mut f = file_name.clone();
                f.set_extension("h");
                f.file_name().unwrap().to_str().unwrap().to_string()
            }));

            for f in functions {
                f.gen(&mut buf_content, &mut buf_header);
            }
        }

        (buf_content.to_string(), buf_header.to_string())
    }
}
fn convert_function<'a>(id: u32, f: &'a CMFunction, list: &MDL<'a>) -> CPPFunction<'a> {
    let mut param_i = 0;
    CPPFunction {
        generated_name: list.function_names[id as usize],
        params: f
            .info
            .params
            .iter()
            .map(|param| CPPType::from_cmtype(param, &list))
            .map(|ty| CPPVarDeclare {
                name: NameId::NameId("local", {
                    param_i += 1;
                    param_i - 1
                }),
                ty,
            })
            .collect(),
        locals: f
            .locals
            .iter()
            .skip(f.info.params.len())
            .map(|local| CPPType::from_cmtype(&local.ty, &list))
            .map(|ty| CPPVarDeclare {
                name: NameId::NameId("local", {
                    param_i += 1;
                    param_i - 1
                }),
                ty,
            })
            .collect(),
        ty_return: CPPType::from_cmtype(&f.info.ty_return, &list),
        block: {
            let mut block = f
                .block
                .iter()
                .map(|expr| convert_statement(expr, &list))
                .collect::<Vec<_>>();

            if let Some(last) = block.last_mut() {
                if let CPPStatement::Expr { expr } = last {
                    *last = CPPStatement::Return { expr: expr.clone() };
                }
            }

            block
        },
    }
}
fn localvar_name<'a>(id: usize) -> NameId<'a> {
    NameId::NameId("local", id as u32)
}
fn convert_statement<'a>(expr: &'a CMExpression, list: &MDL<'a>) -> CPPStatement<'a> {
    dbg!(expr);
    match expr {
        CMExpression::AssignVar { ident, value } => CPPStatement::VarAssign {
            name: localvar_name(*ident),
            expr: convert_expr(&*value, list),
        },
        expr => CPPStatement::Expr {
            expr: convert_expr(expr, list),
        },
    }
}
fn convert_expr<'a>(expr: &'a CMExpression, list: &MDL<'a>) -> CPPExpr<'a> {
    match expr {
        CMExpression::ReadVar { ident } => CPPExpr::VarRead {
            name: localvar_name(*ident),
        },
        CMExpression::Call {
            function_id,
            arguments,
            always_inline,
            inlined_lambdas,
        } => {
            let id = function_id.reify_id(CPPCodegenBackend::PLATFORM_INFO);
            CPPExpr::Call {
                name: list.lookup_function(id).1,
                args: arguments
                    .iter()
                    .map(|expr| convert_expr(expr, list))
                    .collect(),
            }
        }
        CMExpression::LiteralValue(literal) => CPPExpr::SingleToken {
            token: match literal {
                CMLiteralValue::Bool(v) => CPPToken::LBool(*v),
                CMLiteralValue::Int(v) => CPPToken::LInt(*v),
                CMLiteralValue::Float(v) => CPPToken::LFloat(*v),
                CMLiteralValue::String(v) => CPPToken::LString(v.as_str()),
                CMLiteralValue::Complex(v) => todo!(),
            },
        },
        v => {
            dbg!(v);
            todo!()
        }
    }
}

struct CPPCodegenStateTypes<'a>(PhantomData<&'a ()>);
impl<'a> MDLTypes<'a> for CPPCodegenStateTypes<'a> {
    type Function = CPPFunction<'a>;
    type Struct = ();
    type DataRef = NameId<'a>;
    fn function_name(i: u32, f: &'a CMFunction) -> Self::DataRef {
        if let Some(name) = f.info.attrs.iter().find_map(|attr| {
            if attr.name == "name" {
                match &attr.object {
                    MacroObject::Ident(name) => Some(name.as_str()),
                    _ => None,
                }
            } else {
                None
            }
        }) {
            NameId::Name(name)
        } else {
            NameId::NameId("fn", i)
        }
    }
    fn struct_name(i: u32, f: &'a CMStruct) -> Self::DataRef {
        NameId::NameId("Struct", i)
    }
}
type MDL<'a> = MergedDataList<'a, CPPCodegenStateTypes<'a>>;

struct CPPCodegenState<'a> {
    list: &'a MergedDataList<'a, CPPCodegenStateTypes<'a>>,
    current_locals: &'a [CPPVarDeclare<'a>],
}

define_codegen_token_enum! {
    CPPToken<'a, CPPCodegenState<'a>>;
    // :::::: General
    Name(name: &'a str) [space_sensitive = true] => *name,
    NameId(name_id: (&'a str, [&'static str; 8])) [space_sensitive = true] =>
        name_id.0 ;
        name_id.1[0] ; name_id.1[1] ; name_id.1[2] ; name_id.1[3] ;
        name_id.1[4] ; name_id.1[5] ; name_id.1[6] ; name_id.1[7],
    /// Raw code to write directly to the buffer.
    Raw(code: &'a str) => *code,
    StringEscapedRaw(code: &'a str) => code.escape_debug().to_string(),

    // :::::: [L]iterals
    LBool(v: bool) [space_sensitive = true] => v.to_string(),
    LInt(v: i128) [space_sensitive = true] => v.to_string(),
    LFloat(v: f64) [space_sensitive = true] => v.to_string(),
    LString(v: &'a str) => "\""; v.escape_debug().to_string(); "\"",

    // :::::: [K]eywords
    KClass [space_sensitive = true] => "class",
    KReturn [space_sensitive = true] => "return",

    // :::::: [S]eparators
    SSemicolon => ";",
    SComma => ",",

    // :::::: [O]perations
    OSet => "=",

    // :::::: [P]arentheses
    PParenOpen => "(",
    PParenClose => ")",
    PSquareOpen => "[",
    PSquareClose => "]",
    PCurlyOpen => "{",
    PCurlyClose => "}",
    PAngleOpen => "<",
    PAngleClose => ">",

    // :::::: [A]roperty accessors
    AStruct => ".",
    APointer => "->",
    AType => "::",

    // :::::: [P]ointers
    PDeref => "*",
    PRef => "&",

    // :::::: [M]acros
    MInclude => "#include",
}

trait CPPCodegenTopLevel<'a> {
    fn gen(&self, buf_content: &mut CodeBuf<'a>, buf_header: &mut CodeBuf<'a>);
}

fn to_hex_sequence_u32(v: u32) -> [&'static str; 8] {
    const HEX_STRS: [&'static str; 16] = [
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f",
    ];
    // Safety: [[T;M];N] -> [T;M*N] is safe because array memory is simple and
    // always aligned and in order with no headers or uneven breaks.
    let mut data: [&'static str; 8] = unsafe {
        std::mem::transmute(
            v.to_be_bytes()
                .map(|v| [HEX_STRS[(v >> 4) as usize], HEX_STRS[(v & 0x0f) as usize]]),
        )
    };
    for i in 0..(data.len() - 1) {
        if data[i].as_bytes()[0] == "0".as_bytes()[0] {
            data[i] = "";
        } else {
            break;
        }
    }
    data
}

struct CPPFunction<'a> {
    generated_name: NameId<'a>,
    params: Vec<CPPVarDeclare<'a>>,
    locals: Vec<CPPVarDeclare<'a>>,
    ty_return: CPPType<'a>,
    block: Vec<CPPStatement<'a>>,
}
impl<'a> CPPFunction<'a> {
    fn gen_heading(&self, buf: &mut CodeBuf<'a>) {
        buf.write_newline();
        buf.write(&self.ty_return);
        buf.write(&self.generated_name);
        buf.write(&CPPToken::PParenOpen);
        if !self.params.is_empty() {
            for v in &self.params[..self.params.len() - 1] {
                buf.write(v);
                buf.write(&CPPToken::SComma);
            }
            buf.write(&self.params[self.params.len() - 1])
        }
        buf.write(&CPPToken::PParenClose);
    }
    fn gen_body(&self, buf: &mut CodeBuf<'a>) {
        buf.write(&CPPToken::PCurlyOpen);
        for v in &self.locals {
            buf.write(v);
            buf.write(&CPPToken::SSemicolon);
        }
        for v in &self.block {
            buf.write(v);
        }
        buf.write(&CPPToken::PCurlyClose);
    }
}
impl<'a> CPPCodegenTopLevel<'a> for CPPFunction<'a> {
    fn gen(&self, buf_content: &mut CodeBuf<'a>, buf_header: &mut CodeBuf<'a>) {
        {
            self.gen_heading(buf_header);
            buf_header.write(&CPPToken::SSemicolon);
        }
        {
            self.gen_heading(buf_content);
            self.gen_body(buf_content);
        }
    }
}
#[derive(Debug, Clone)]
enum CPPExpr<'a> {
    VarRead {
        name: NameId<'a>,
    },
    Call {
        name: NameId<'a>,
        args: Vec<CPPExpr<'a>>,
    },
    SingleToken {
        token: CPPToken<'a>,
    },
}
impl<'a> Codegen<'a> for CPPExpr<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        match self {
            Self::VarRead { name } => buf.write(name),
            Self::Call { name, args } => {
                buf.write(name);
                buf.write(&CPPToken::PParenOpen);
                if args.len() > 0 {
                    for arg in &args[..args.len() - 1] {
                        buf.write(arg);
                        buf.write(&CPPToken::SComma);
                    }
                    buf.write(&args[args.len() - 1]);
                }
                buf.write(&CPPToken::PParenClose);
            }
            Self::SingleToken { token } => buf.write(token),
        }
    }
}
#[derive(Debug, Clone)]
enum CPPStatement<'a> {
    VarAssign { name: NameId<'a>, expr: CPPExpr<'a> },
    Expr { expr: CPPExpr<'a> },
    Return { expr: CPPExpr<'a> },
}
impl<'a> Codegen<'a> for CPPStatement<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        match self {
            Self::VarAssign { name, expr } => {
                buf.write(name);
                buf.write(&CPPToken::OSet);
                buf.write(expr);
            }
            Self::Expr { expr } => buf.write(expr),
            Self::Return { expr } => {
                buf.write(&CPPToken::KReturn);
                buf.write(expr);
            }
        }
        buf.write(&CPPToken::SSemicolon);
    }
}

#[derive(Debug, Clone, Copy)]
struct CPPType<'a> {
    /// the text name of this type
    name: NameId<'a>,
    /// whether this is an array, and if it is, the length
    array: Option<u32>,
    /// number of pointer layers
    pointer: u8,
}
impl<'a> CPPType<'a> {
    fn from_cmtype(ty: &CMType, list: &MDL<'a>) -> Self {
        let name = match ty {
            CMType::Unknown => NameId::Name("void"),
            CMType::Never => NameId::Name("void"),
            CMType::Void => NameId::Name("void"),
            CMType::Int => NameId::Name("int"),
            CMType::Float => NameId::Name("double"),
            CMType::Complex => todo!(),
            CMType::Bool => NameId::Name("bool"),
            CMType::String => todo!(),
            CMType::FunctionRef { params, return_ty } => todo!(),
            CMType::StructData(id) => list.lookup_struct(*id).1,
            CMType::StructInstance(_) => todo!(),
            CMType::Tuple(_) => todo!(),
        };
        CPPType {
            name,
            array: None,
            pointer: 0,
        }
    }
}
impl<'a> Codegen<'a> for CPPType<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        buf.write(&self.name);
        for _ in 0..self.pointer {
            buf.write(&CPPToken::PDeref);
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CPPVarDeclare<'a> {
    name: NameId<'a>,
    ty: CPPType<'a>,
}
impl<'a> Codegen<'a> for CPPVarDeclare<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        buf.write(&self.ty);

        buf.write(&self.name);

        if let Some(array_n) = self.ty.array {
            buf.write_tokens([
                CPPToken::PSquareOpen,
                CPPToken::LInt(array_n.into()),
                CPPToken::PSquareClose,
            ]);
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum NameId<'a> {
    Name(&'a str),
    NameId(&'a str, u32),
}
impl<'a> Codegen<'a> for NameId<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        match *self {
            Self::Name(name) => buf.write(&CPPToken::Name(name)),
            Self::NameId(name, id) => buf.write(&CPPToken::NameId((name, to_hex_sequence_u32(id)))),
        }
    }
}

#[derive(Debug, Clone)]
enum CPPHeader<'a> {
    Local(&'a str),
    LocalOwned(String),
    Library(&'a str),
}
impl<'a> Codegen<'a> for CPPHeader<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        buf.write(&CPPToken::MInclude);
        match self {
            Self::Local(name) => buf.write(&CPPToken::LString(name)),
            Self::LocalOwned(name) => {
                let name = std::mem::ManuallyDrop::new(name);
                buf.write_tokens([
                    CPPToken::Raw("\""),
                    // extending lifetime, safe because write tokens copies the data before this is manually dropped.
                    CPPToken::StringEscapedRaw(unsafe { std::mem::transmute(name.as_str()) }),
                    CPPToken::Raw("\""),
                ]);
                std::mem::drop(std::mem::ManuallyDrop::into_inner(name));
            }
            Self::Library(name) => buf.write_tokens([
                CPPToken::PAngleOpen,
                CPPToken::Raw(name),
                CPPToken::PAngleClose,
            ]),
        }
        buf.write(&CPPToken::SSemicolon);
    }
}
