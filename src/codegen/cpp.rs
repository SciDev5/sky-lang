use std::{collections::HashSet, marker::PhantomData, path::PathBuf, rc::Rc, str::FromStr};

use crate::{
    common::{
        backend::{BackendCompiler, BackendId, CommonBackend, PlatformInfo},
        common_module::{CMExpression, CMFunction, CMLiteralValue, CMStruct, CMType, CommonModule},
    },
    define_codegen_token_enum,
    parse::{
        macros::{MacroCall, MacroObject},
        tokenization::{BracketType, SeparatorType},
    },
};

use super::common::{MDLDestructure, MDLTypes, MergedDataList};

pub struct CPPCodegenBackend;
impl BackendCompiler for CPPCodegenBackend {
    const ID: BackendId = 0x0000_6626_86d7_a1fa;
    const PLATFORM_INFO: PlatformInfo = PlatformInfo {
        id: Self::ID,
        name: "codegen_cpp",
        compat_ids: &[Self::ID, CommonBackend::ID],
    };
    type Config = (PathBuf,);
    type Output = (String, String);
    fn compile(&self, source: &Vec<Rc<CommonModule>>, config: (PathBuf,)) -> Self::Output {
        let mut list = MergedDataList::new(source);
        let mut headers: Headers = Headers {
            headers: HashSet::new(),
            leading_code_injects: Vec::new(),
        };

        for (i, cm) in source.iter().map(Rc::as_ref).enumerate() {
            let iters = list.iters();
            MergedDataList::push_code(
                iters
                    .functions()
                    .into_iter()
                    .map(|(id, f)| convert_function(id, f, &list, &mut headers))
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

        let mut buf_content = CodeBuf::new(CPPCodegenState {
            list: &list,
            current_locals: &[],
        });
        let mut buf_header = CodeBuf::new(CPPCodegenState {
            list: &list,
            current_locals: &[],
        });

        {
            for header in &headers.headers {
                buf_header.write(header);
            }

            buf_content.write(&CPPHeader::LocalOwned({
                let mut f = file_name.clone();
                f.set_extension("h");
                f.file_name().unwrap().to_str().unwrap().to_string()
            }));
            for inject in &headers.leading_code_injects {
                buf_content.write(inject);
            }

            for f in functions {
                f.gen(&mut buf_content, &mut buf_header);
            }
        }

        (buf_content.to_string(), buf_header.to_string())
    }
}
fn convert_function<'a>(
    id: u32,
    f: &'a CMFunction,
    list: &MDL<'a>,
    headers: &mut Headers<'a>,
) -> CPPFunction<'a> {
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
                .map(|expr| convert_statement(expr, &list, headers))
                .collect::<Vec<_>>();

            if !matches!(f.info.ty_return, CMType::Void) {
                if let Some(last) = block.last_mut() {
                    if let CPPStatement::Expr { expr } = last {
                        *last = CPPStatement::Return { expr: expr.clone() };
                    }
                }
            }

            block
        },
    }
}
fn localvar_name<'a>(id: usize) -> NameId<'a> {
    NameId::NameId("local", id as u32)
}
fn convert_macro_call<'a>(
    call: &'a MacroCall<CMExpression>,
    list: &MDL<'a>,
    headers: &mut Headers<'a>,
) -> CPPExprOrStatement<'a> {
    use CPPExprOrStatement::*;
    fn gen_emit<'a>(
        str: &'a String,
        interp: &'a Option<Vec<(CMExpression, String)>>,
        list: &MDL<'a>,
        headers: &mut Headers<'a>,
    ) -> CPPExpr<'a> {
        CPPExpr::Raw {
            token: CPPToken::Raw(str.as_str()),
            trail: interp.as_ref().map(|interp| {
                interp
                    .iter()
                    .map(|(expr, str)| {
                        (
                            convert_expr(&expr, list, headers),
                            CPPToken::Raw(str.as_str()),
                        )
                    })
                    .collect()
            }),
        }
    }
    match (call.name.as_str(), &call.object) {
        (
            "include",
            MacroObject::Listlike {
                ty: BracketType::Square,
                children,
            },
        ) => {
            for child in children {
                match child {
                    (
                        MacroObject::Ident(str) | MacroObject::String(str, None),
                        None | Some(SeparatorType::Comma),
                    ) => {
                        headers.headers.insert(CPPHeader::Library(str.as_str()));
                    }
                    _ => todo!("// TODO handle incorrect/unknown macros"),
                }
            }
            Statement(CPPStatement::Empty)
        }
        ("emit_at_top", MacroObject::String(str, interp)) => {
            let emit = gen_emit(str, interp, list, headers);
            headers.leading_code_injects.push(emit);
            Statement(CPPStatement::Empty)
        }
        ("emit_stmt", MacroObject::String(str, interp)) => Statement(CPPStatement::Expr {
            expr: gen_emit(str, interp, list, headers),
        }),
        ("emit", MacroObject::String(str, interp)) => Expr(gen_emit(str, interp, list, headers)),
        v => {
            dbg!(v);
            todo!("// TODO handle unknown macros")
        }
    }
}
fn convert_statement<'a>(
    expr: &'a CMExpression,
    list: &MDL<'a>,
    headers: &mut Headers<'a>,
) -> CPPStatement<'a> {
    match expr {
        CMExpression::InlineMacroCall { call, .. } => {
            convert_macro_call(call, list, headers).into_statement()
        }
        CMExpression::AssignVar { ident, value } => CPPStatement::VarAssign {
            name: localvar_name(*ident),
            expr: convert_expr(&*value, list, headers),
        },

        CMExpression::Fail => CPPStatement::ExprVoid {
            expr: CPPExpr::Raw {
                token: CPPToken::Raw("while true{Serial.println(\"FAILED\");}"),
                trail: None,
            },
        },
        CMExpression::FailAfter(code) => {
            let mut trail = code[..code.len().checked_sub(1).unwrap_or(0)]
                .iter()
                .map(|expr| (convert_expr(expr, list, headers), CPPToken::Raw(";")))
                .collect::<Vec<_>>();
            for expr in code.last().into_iter() {
                trail.push((
                    convert_expr(expr, list, headers),
                    CPPToken::Raw("while true{Serial.println(\"FAILED\");}"),
                ))
            }
            CPPStatement::ExprVoid {
                expr: CPPExpr::Raw {
                    token: CPPToken::Raw(""),
                    trail: Some(trail),
                },
            }
        }
        expr => CPPStatement::Expr {
            expr: convert_expr(expr, list, headers),
        },
    }
}
fn convert_expr<'a>(
    expr: &'a CMExpression,
    list: &MDL<'a>,
    headers: &mut Headers<'a>,
) -> CPPExpr<'a> {
    match expr {
        CMExpression::InlineMacroCall { call, .. } => {
            convert_macro_call(call, list, headers).into_expr()
        }
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
                    .map(|expr| convert_expr(expr, list, headers))
                    .collect(),
            }
        }
        CMExpression::LiteralValue(literal) => CPPExpr::Raw {
            token: match literal {
                CMLiteralValue::Bool(v) => CPPToken::LBool(*v),
                CMLiteralValue::Int(v) => CPPToken::LInt(*v),
                CMLiteralValue::Float(v) => CPPToken::LFloat(*v),
                CMLiteralValue::String(v) => CPPToken::LString(v.as_str()),
                CMLiteralValue::Complex(v) => todo!(),
            },
            trail: None,
        },
        CMExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => {
            let else_block = else_block.as_ref().map(|it| it.as_slice()).unwrap_or(&[]);
            let mut else_block = convert_subblock(else_block, list, headers);
            for (condition, block) in elifs.iter().rev() {
                else_block = CPPExpr::Ternary {
                    condition: Box::new(convert_expr(condition, list, headers)),
                    positive: Box::new(convert_subblock(&block[..], list, headers)),
                    negative: Box::new(else_block),
                };
            }
            CPPExpr::Ternary {
                condition: Box::new(convert_expr(&*condition, list, headers)),
                positive: Box::new(convert_subblock(block, list, headers)),
                negative: Box::new(else_block),
            }
        }
        CMExpression::Fail => CPPExpr::Raw {
            token: CPPToken::Raw("({while true{Serial.println(\"FAILED\");}})"),
            trail: None,
        },
        CMExpression::FailAfter(code) => {
            let mut trail = code[..code.len().checked_sub(1).unwrap_or(0)]
                .iter()
                .map(|expr| (convert_expr(expr, list, headers), CPPToken::Raw(";")))
                .collect::<Vec<_>>();
            for expr in code.last().into_iter() {
                trail.push((
                    convert_expr(expr, list, headers),
                    CPPToken::Raw(";while true{Serial.println(\"FAILED\");}})"),
                ))
            }
            CPPExpr::Raw {
                token: CPPToken::Raw("({"),
                trail: Some(trail),
            }
        }
        v => {
            dbg!(v);
            todo!()
        }
    }
}
fn convert_subblock<'a>(
    block: &'a [CMExpression],
    list: &MDL<'a>,
    headers: &mut Headers<'a>,
) -> CPPExpr<'a> {
    if block.len() == 1 {
        let statement = convert_statement(&block[0], list, headers);
        match statement {
            CPPStatement::Expr { expr } => expr,
            v => CPPExpr::StatementExpr {
                statements: vec![v],
            },
        }
    } else {
        CPPExpr::StatementExpr {
            statements: block
                .iter()
                .map(|statment| convert_statement(statment, list, headers))
                .collect(),
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
                    MacroObject::String(name, None) => Some(name.as_str()),
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

struct Headers<'a> {
    headers: HashSet<CPPHeader<'a>>,
    leading_code_injects: Vec<CPPExpr<'a>>,
}

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
    SColon => ":",

    // :::::: [O]perations
    OSet => "=",
    OTernary => "?",

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
#[test]
fn k() {
    let seq = dbg!(to_hex_sequence_u32(16));
    let mut buf = CodeBuf::new(CPPCodegenState {
        list: unsafe { std::mem::transmute(&()) },
        current_locals: &[],
    });
    buf.write(&CPPToken::NameId(("x", seq)));
    dbg!(buf.to_string());
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
    StatementExpr {
        statements: Vec<CPPStatement<'a>>,
    },
    Ternary {
        condition: Box<CPPExpr<'a>>,
        positive: Box<CPPExpr<'a>>,
        negative: Box<CPPExpr<'a>>,
    },
    Raw {
        token: CPPToken<'a>,
        trail: Option<Vec<(CPPExpr<'a>, CPPToken<'a>)>>,
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
            Self::Raw { token, trail } => {
                buf.write_space_sensitive_update(true);
                buf.write_space_sensitive_update(false);
                buf.write(token);
                if let Some(trail) = trail {
                    for (expr, token) in trail {
                        buf.write(expr);
                        buf.write(token);
                    }
                }
                buf.write_space_sensitive_update(false);
                buf.write_space_sensitive_update(true);
            }
            Self::StatementExpr { statements } => {
                buf.write_tokens([CPPToken::PParenOpen, CPPToken::PCurlyOpen]);
                for statement in statements {
                    buf.write(statement);
                }
                buf.write_tokens([CPPToken::PCurlyClose, CPPToken::PParenClose]);
            }
            Self::Ternary {
                condition,
                positive,
                negative,
            } => {
                buf.write(condition.as_ref());
                buf.write(&CPPToken::OTernary);
                buf.write(positive.as_ref());
                buf.write(&CPPToken::SColon);
                buf.write(negative.as_ref());
            }
        }
    }
}
#[derive(Debug, Clone)]
enum CPPStatement<'a> {
    Empty,
    VarAssign { name: NameId<'a>, expr: CPPExpr<'a> },
    Expr { expr: CPPExpr<'a> },
    ExprVoid { expr: CPPExpr<'a> },
    Return { expr: CPPExpr<'a> },
}
impl<'a> Codegen<'a> for CPPStatement<'a> {
    fn gen(&self, buf: &mut CodeBuf<'a>) {
        match self {
            Self::Empty => return,
            Self::VarAssign { name, expr } => {
                buf.write(name);
                buf.write(&CPPToken::OSet);
                buf.write(expr);
            }
            Self::Expr { expr } => buf.write(expr),
            Self::ExprVoid { expr } => buf.write(expr),
            Self::Return { expr } => {
                buf.write(&CPPToken::KReturn);
                buf.write(expr);
            }
        }
        buf.write(&CPPToken::SSemicolon);
    }
}
enum CPPExprOrStatement<'a> {
    Expr(CPPExpr<'a>),
    Statement(CPPStatement<'a>),
}
impl<'a> CPPExprOrStatement<'a> {
    fn into_expr(self) -> CPPExpr<'a> {
        match self {
            Self::Expr(expr) => expr,
            Self::Statement(stmt) => CPPExpr::StatementExpr {
                statements: vec![stmt],
            },
        }
    }
    fn into_statement(self) -> CPPStatement<'a> {
        match self {
            Self::Expr(expr) => CPPStatement::Expr { expr },
            Self::Statement(stmt) => stmt,
        }
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
            CMType::String => NameId::Name("String"),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        buf.write_tokens([CPPToken::SSemicolon, CPPToken::Raw("\n")]);
    }
}
