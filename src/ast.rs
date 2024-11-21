type Pos = usize;

pub enum NativeType {
    Int,
}

pub enum TypeInner {
    NativeType(NativeType),
    StructType(String),
}

pub struct Type {
    pub pos: Pos,
    pub inner: Box<TypeInner>,
}

pub struct RightValList {
    pub head: Box<RightVal>,
    pub next: Option<Box<RightValList>>,
}

pub struct FnCall {
    pub pos: Pos,
    pub name: String,
    pub vals: Box<RightValList>,
}

pub enum IndexExprInner {
    Num(usize),
    Id(String),
}

pub struct IndexExpr {
    pub pos: Pos,
    pub inner: Box<IndexExprInner>,
}

pub struct ArrayExpr {
    pub pos: Pos,
    pub arr: Box<LeftVal>,
    pub idx: Box<IndexExpr>,
}

pub struct MemberExpr {
    pub pos: Pos,
    pub id: String,
    pub member_id: String,
}

pub enum ExprUnitInner {
    Num(i32),
    Id(String),
    Arith(Box<ArithExpr>),
    FnCall(Box<FnCall>),
    ArrayExpr(Box<ArrayExpr>),
    MemberExpr(Box<MemberExpr>),
    ArithUExpr(Box<ArithUExpr>),
}

pub struct ExprUnit {
    pub pos: Pos,
    pub inner: ExprUnitInner,
}

pub enum ArithUOp {
    Neg,
}

pub enum ArithBiOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum BoolUOp {
    Not,
}

pub enum BoolBiOp {
    And,
    Or,
}

pub enum ComOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

pub struct ArithBiOpExpr {
    pub pos: Pos,
    pub op: ArithBiOp,
    pub left: Box<ArithExpr>,
    pub right: Box<ArithExpr>,
}

pub struct ArithUExpr {
    pub pos: Pos,
    pub op: ArithUOp,
    pub expr: Box<ExprUnit>,
}

pub enum ArithExprInner {
    ArithBiOpExpr(ArithBiOpExpr),
    ExprUnit(ExprUnit),
}

pub struct ArithExpr {
    pub pos: Pos,
    pub inner: Box<ArithExprInner>,
}

pub struct BoolBiOpExpr {
    pub pos: Pos,
    pub op: BoolBiOp,
    pub left: Box<BoolExpr>,
    pub right: Box<BoolExpr>,
}

pub struct BoolUOpExpr {
    pub pos: Pos,
    pub op: BoolUOp,
    pub cond: Box<BoolUnit>,
}

pub enum BoolExprInner {
    BoolBiOpExpr(BoolBiOpExpr),
    BoolUnit(BoolUnit),
}

pub struct BoolExpr {
    pub pos: Pos,
    pub inner: Box<BoolExprInner>,
}

pub struct ComExpr {
    pub pos: Pos,
    pub op: ComOp,
    pub left: ExprUnit,
    pub right: ExprUnit,
}

pub enum BoolUnitInner {
    ComExpr(ComExpr),
    BoolExpr(BoolExpr),
    BoolUOpExpr(BoolUOpExpr),
}

pub struct BoolUnit {
    pub pos: Pos,
    pub inner: Box<BoolUnitInner>,
}

pub enum RightValInner {
    ArithExpr(ArithExpr),
    BoolExpr(BoolExpr),
}

pub struct RightVal {
    pub pos: Pos,
    pub inner: Box<RightValInner>,
}

pub enum LeftValInner {
    Id(String),
    ArrayExpr(ArrayExpr),
    MemberExpr(MemberExpr),
}

pub struct LeftVal {
    pub pos: Pos,
    pub inner: Box<LeftValInner>,
}

pub struct AssignmentStmt {
    pub pos: Pos,
    pub left_val: Box<LeftVal>,
    pub right_val: Box<RightVal>,
}

pub struct VarDeclScalar {
    pub pos: Pos,
    pub id: String,
    pub val_type: Box<Type>,
}

pub struct VarDeclArray {
    pub pos: Pos,
    pub id: String,
    pub len: usize,
    pub val_type: Box<Type>,
}

pub enum VarDeclInner {
    Scalar(VarDeclScalar),
    Array(VarDeclArray),
}

pub struct VarDecl {
    pub pos: Pos,
    pub inner: Box<VarDeclInner>,
}

pub enum VarDefInner {
    Scalar(VarDefScalar),
    Array(VarDefArray),
}

pub struct VarDef {
    pub pos: Pos,
    pub inner: Box<VarDefInner>,
}

pub struct VarDefScalar {
    pub pos: Pos,
    pub id: String,
    pub valtype: Box<Type>,
    pub val: Box<RightVal>,
}

pub struct VarDefArray {
    pub pos: Pos,
    pub id: String,
    pub len: usize,
    pub valtype: Box<Type>,
    pub vals: Box<RightValList>,
}

pub enum VarDeclStmtInner {
    Decl(VarDecl),
    Def(VarDef),
}

pub struct VarDeclStmt {
    pub pos: Pos,
    pub inner: Box<VarDeclStmtInner>,
}

pub struct VarDeclList {
    pub head: Box<VarDecl>,
    pub next: Option<Box<VarDeclList>>,
}

pub struct StructDef {
    pub pos: Pos,
    pub id: String,
    pub decls: Box<VarDeclList>,
}

pub struct FnDecl {
    pub pos: Pos,
    pub id: String,
    pub param_decl: Box<ParamDecl>,
    pub ret_type: Box<Type>,
}

pub struct ParamDecl {
    pub decls: Box<VarDeclList>,
}

pub struct FnDef {
    pub pos: Pos,
    pub fn_decl: Box<FnCall>,
    pub stmts: Box<CodeBlockStmtList>,
}

pub struct IfStmt {
    pub pos: Pos,
    pub bool_unit: Box<BoolUnit>,
    pub if_stmts: Box<CodeBlockStmtList>,
    pub else_stmts: Box<CodeBlockStmtList>,
}

pub struct WhileStmt {
    pub pos: Pos,
    pub bool_unit: Box<BoolUnit>,
    pub while_stmts: Box<CodeBlockStmtList>,
}

pub struct CallStmt {
    pub pos: Pos,
    pub fn_call: Box<FnCall>,
}

pub struct ReturnStmt {
    pub pos: Pos,
    pub val: Box<RightVal>,
}

pub enum CodeBlockStmtInner {
    VarDecl(VarDeclStmt),
    Assignment(AssignmentStmt),
    Call(CallStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
}

pub struct CodeBlockStmt {
    pub pos: Pos,
    pub inner: Box<CodeBlockStmtInner>,
}

pub struct CodeBlockStmtList {
    pub head: Box<CodeBlockStmt>,
    pub next: Option<Box<CodeBlockStmtList>>,
}

pub struct FnDeclStmt {
    pub pos: Pos,
    pub fn_decl: Box<FnDecl>,
}

pub struct Program {
    pub elements: Box<ProgramElementList>,
}

pub struct ProgramElementList {
    pub element: Box<ProgramElement>,
    pub next: Option<Box<ProgramElementList>>,
}

pub enum ProgramElementInner {
    VarDecl(Box<VarDeclStmt>),
    StructDef(Box<StructDef>),
    FnDecl(Box<FnDeclStmt>),
    FnDef(Box<FnDef>),
}

pub struct ProgramElement {
    pub inner: ProgramElementInner,
}
