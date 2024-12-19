use std::rc::Rc;

type Pos = usize;

#[derive(Debug)]
pub enum NativeType {
    Int,
}

#[derive(Debug)]
pub enum TypeInner {
    NativeType(Box<NativeType>),
    StructType(Box<String>),
}

#[derive(Debug)]
pub struct Type {
    pub pos: Pos,
    pub inner: TypeInner,
}

#[derive(Debug)]
pub struct RightValList {
    pub head: Box<RightVal>,
    pub next: Option<Box<RightValList>>,
}

impl RightValList {
    pub fn iter(&self) -> RightValListIterator {
        RightValListIterator {
            current: Some(self),
        }
    }
}

pub struct RightValListIterator<'a> {
    current: Option<&'a RightValList>,
}

impl<'a> Iterator for RightValListIterator<'a> {
    type Item = &'a RightVal;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.take().map(|node| {
            self.current = node.next.as_ref().map(|next_node| &**next_node);
            &*node.head
        })
    }
}

#[derive(Debug)]
pub struct FnCall {
    pub pos: Pos,
    pub name: String,
    pub vals: Option<Box<RightValList>>,
}

#[derive(Debug)]
pub enum IndexExprInner {
    Num(usize),
    Id(String),
}

#[derive(Debug)]
pub struct IndexExpr {
    pub pos: Pos,
    pub inner: IndexExprInner,
}

#[derive(Debug)]
pub struct ArrayExpr {
    pub pos: Pos,
    pub arr: Box<LeftVal>,
    pub idx: Box<IndexExpr>,
}

#[derive(Debug)]
pub struct MemberExpr {
    pub pos: Pos,
    pub struct_id: Box<LeftVal>,
    pub member_id: String,
}

#[derive(Debug)]
pub enum ExprUnitInner {
    Num(i32),
    Id(String),
    ArithExpr(Box<ArithExpr>),
    FnCall(Box<FnCall>),
    ArrayExpr(Box<ArrayExpr>),
    MemberExpr(Box<MemberExpr>),
    ArithUExpr(Box<ArithUExpr>),
}

#[derive(Debug)]
pub struct ExprUnit {
    pub pos: Pos,
    pub inner: ExprUnitInner,
}

#[derive(Debug, PartialEq)]
pub enum ArithUOp {
    Neg,
}

#[derive(Debug)]
pub enum ArithBiOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum BoolUOp {
    Not,
}

#[derive(Debug, PartialEq)]
pub enum BoolBiOp {
    And,
    Or,
}

#[derive(Debug)]
pub enum ComOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

#[derive(Debug)]
pub struct ArithBiOpExpr {
    pub pos: Pos,
    pub op: ArithBiOp,
    pub left: Box<ArithExpr>,
    pub right: Box<ArithExpr>,
}

#[derive(Debug)]
pub struct ArithUExpr {
    pub pos: Pos,
    pub op: ArithUOp,
    pub expr: Box<ExprUnit>,
}

#[derive(Debug)]
pub enum ArithExprInner {
    ArithBiOpExpr(Box<ArithBiOpExpr>),
    ExprUnit(Box<ExprUnit>),
}

#[derive(Debug)]
pub struct ArithExpr {
    pub pos: Pos,
    pub inner: ArithExprInner,
}

#[derive(Debug)]
pub struct BoolBiOpExpr {
    pub pos: Pos,
    pub op: BoolBiOp,
    pub left: Box<BoolExpr>,
    pub right: Box<BoolExpr>,
}

#[derive(Debug)]
pub struct BoolUOpExpr {
    pub pos: Pos,
    pub op: BoolUOp,
    pub cond: Box<BoolUnit>,
}

#[derive(Debug)]
pub enum BoolExprInner {
    BoolBiOpExpr(Box<BoolBiOpExpr>),
    BoolUnit(Box<BoolUnit>),
}

#[derive(Debug)]
pub struct BoolExpr {
    pub pos: Pos,
    pub inner: BoolExprInner,
}

#[derive(Debug)]
pub struct ComExpr {
    pub pos: Pos,
    pub op: ComOp,
    pub left: Box<ExprUnit>,
    pub right: Box<ExprUnit>,
}

#[derive(Debug)]
pub enum BoolUnitInner {
    ComExpr(Box<ComExpr>),
    BoolExpr(Box<BoolExpr>),
    BoolUOpExpr(Box<BoolUOpExpr>),
}

#[derive(Debug)]
pub struct BoolUnit {
    pub pos: Pos,
    pub inner: BoolUnitInner,
}

#[derive(Debug)]
pub enum RightValInner {
    ArithExpr(Box<ArithExpr>),
    BoolExpr(Box<BoolExpr>),
}

#[derive(Debug)]
pub struct RightVal {
    pub pos: Pos,
    pub inner: RightValInner,
}

#[derive(Debug)]
pub enum LeftValInner {
    Id(String),
    ArrayExpr(Box<ArrayExpr>),
    MemberExpr(Box<MemberExpr>),
}

#[derive(Debug)]
pub struct LeftVal {
    pub pos: Pos,
    pub inner: LeftValInner,
}

#[derive(Debug)]
pub struct AssignmentStmt {
    pub pos: Pos,
    pub left_val: Box<LeftVal>,
    pub right_val: Box<RightVal>,
}

#[derive(Debug)]
pub struct VarDeclScalar {
    pub pos: Pos,
}

#[derive(Debug)]
pub struct VarDeclArray {
    pub pos: Pos,
    pub len: isize,
}

#[derive(Debug)]
pub enum VarDeclInner {
    Scalar(Box<VarDeclScalar>),
    Array(Box<VarDeclArray>),
}

#[derive(Debug)]
pub struct VarDecl {
    pub pos: Pos,
    pub id: String,
    pub real_type: Rc<Option<Type>>,
    pub inner: VarDeclInner,
}

#[derive(Debug)]
pub enum VarDefInner {
    Scalar(Box<VarDefScalar>),
    Array(Box<VarDefArray>),
}

#[derive(Debug)]
pub struct VarDef {
    pub pos: Pos,
    pub id: String,
    pub real_type: Rc<Option<Type>>,
    pub inner: VarDefInner,
}

#[derive(Debug)]
pub struct VarDefScalar {
    pub pos: Pos,
    pub val: Box<RightVal>,
}

#[derive(Debug)]
pub struct VarDefArray {
    pub pos: Pos,
    pub len: isize,
    pub vals: Box<RightValList>,
}

#[derive(Debug)]
pub enum VarDeclStmtInner {
    Decl(Box<VarDecl>),
    Def(Box<VarDef>),
}

impl VarDeclStmtInner {
    pub fn get_id(&self) -> &String {
        match self {
            VarDeclStmtInner::Decl(decl) => &decl.id,
            VarDeclStmtInner::Def(def) => &def.id,
        }
    }

    pub fn get_type(&self) -> &Rc<Option<Type>> {
        match self {
            VarDeclStmtInner::Decl(decl) => &decl.real_type,
            VarDeclStmtInner::Def(def) => &def.real_type,
        }
    }
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub pos: Pos,
    pub inner: VarDeclStmtInner,
}

#[derive(Debug)]
pub struct VarDeclList {
    pub head: Box<VarDecl>,
    pub next: Option<Box<VarDeclList>>,
}

pub struct VarDeclListIterator<'a> {
    current: Option<&'a VarDeclList>,
}

impl<'a> Iterator for VarDeclListIterator<'a> {
    type Item = &'a VarDecl;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.take().map(|node| {
            self.current = node.next.as_ref().map(|next_node| &**next_node);
            &*node.head
        })
    }
}

impl VarDeclList {
    pub fn iter(&self) -> VarDeclListIterator {
        VarDeclListIterator {
            current: Some(self),
        }
    }
}

#[derive(Debug)]
pub struct StructDef {
    pub pos: Pos,
    pub id: String,
    pub decls: Box<VarDeclList>,
}

#[derive(Debug)]
pub struct FnDecl {
    pub pos: Pos,
    pub id: String,
    pub param_decl: Option<Box<ParamDecl>>,
    pub ret_type: Rc<Option<Type>>,
}

#[derive(Debug)]
pub struct ParamDecl {
    pub decls: Box<VarDeclList>,
}

#[derive(Debug)]
pub struct FnDef {
    pub pos: Pos,
    pub fn_decl: Box<FnDecl>,
    pub stmts: Box<CodeBlockStmtList>,
}

#[derive(Debug)]
pub struct IfStmt {
    pub pos: Pos,
    pub bool_unit: Box<BoolUnit>,
    pub if_stmts: Box<CodeBlockStmtList>,
    pub else_stmts: Option<Box<CodeBlockStmtList>>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub pos: Pos,
    pub bool_unit: Box<BoolUnit>,
    pub stmts: Box<CodeBlockStmtList>,
}

#[derive(Debug)]
pub struct CallStmt {
    pub pos: Pos,
    pub fn_call: Box<FnCall>,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub pos: Pos,
    pub val: Option<Box<RightVal>>,
}

#[derive(Debug)]
pub struct ContinueStmt {
    pub pos: Pos,
}

#[derive(Debug)]
pub struct BreakStmt {
    pub pos: Pos,
}

#[derive(Debug)]
pub struct NullStmt {
    pub pos: Pos,
}

#[derive(Debug)]
pub enum CodeBlockStmtInner {
    VarDecl(Box<VarDeclStmt>),
    Assignment(Box<AssignmentStmt>),
    Call(Box<CallStmt>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
    Return(Box<ReturnStmt>),
    Continue(Box<ContinueStmt>),
    Break(Box<BreakStmt>),
    Null(Box<NullStmt>),
}

#[derive(Debug)]
pub struct CodeBlockStmt {
    pub pos: Pos,
    pub inner: CodeBlockStmtInner,
}

#[derive(Debug)]
pub struct CodeBlockStmtList {
    pub head: Box<CodeBlockStmt>,
    pub next: Option<Box<CodeBlockStmtList>>,
}

#[derive(Debug)]
pub struct FnDeclStmt {
    pub pos: Pos,
    pub fn_decl: Box<FnDecl>,
}

#[derive(Debug)]
pub struct Program {
    pub elements: Box<ProgramElementList>,
}

#[derive(Debug)]
pub struct ProgramElementList {
    pub element: Box<ProgramElement>,
    pub next: Option<Box<ProgramElementList>>,
}

impl ProgramElementList {
    pub fn iter(&self) -> ProgramElementListIterator {
        ProgramElementListIterator {
            current: Some(self),
        }
    }
}

pub struct ProgramElementListIterator<'a> {
    current: Option<&'a ProgramElementList>,
}

impl<'a> Iterator for ProgramElementListIterator<'a> {
    type Item = &'a ProgramElement;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.take().map(|node| {
            self.current = node.next.as_ref().map(|next_node| &**next_node);
            &*node.element
        })
    }
}

#[derive(Debug)]
pub enum ProgramElementInner {
    VarDeclStmt(Box<VarDeclStmt>),
    StructDef(Box<StructDef>),
    FnDeclStmt(Box<FnDeclStmt>),
    FnDef(Box<FnDef>),
}

#[derive(Debug)]
pub struct ProgramElement {
    pub inner: ProgramElementInner,
}
