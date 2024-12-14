use crate::ast;
use core::panic;
use indexmap::IndexMap;
use std::sync::atomic::{AtomicI32, Ordering};

pub enum VariableType {
    Int,
    IntPtr,
    Struct,
    StructPtr,
}

pub enum RetType {
    Int,
    Struct,
    Void,
}

pub struct MemberDecl {
    kind: VariableType,
    len: usize,
    struct_name: String,
}

pub struct StructDef {
    name: String,
    members: Vec<MemberDecl>,
}

pub struct FnType {
    ret_type: RetType,
    struct_name: String,
}

pub struct FnDecl {
    name: String,
    args: Vec<MemberDecl>,
    ret: FnType,
}

pub struct VarDef<T>
where
    T: Variable,
{
    name: String,
    var: T,
    init: Vec<i32>,
}

pub enum GlobalDef {
    Struct(StructDef),
    Func(FnDecl),
}

pub struct BlockLabel {
    name: String,
}

impl BlockLabel {
    fn from_index(index: usize) -> Self {
        BlockLabel {
            name: format!("bb{}", index),
        }
    }

    fn from_str(name: &str) -> Self {
        BlockLabel {
            name: name.to_string(),
        }
    }
}

pub trait Variable {
    fn len(&self) -> &isize;
    fn kind(&self) -> &VariableType;
    fn struct_name(&self) -> &Option<String>;
}

pub struct VariableBase {
    kind: VariableType,
    len: isize,
    struct_name: Option<String>,
}

pub struct GlobalVar {
    base: VariableBase,
    name: BlockLabel,
}

pub struct LocalVar {
    base: VariableBase,
    num: i32,
    name: String,
}

// Generic implementation for any struct that contains a `VariableBase`
impl<T> Variable for T
where
    T: AsRef<VariableBase>, // T must provide a reference to a `VariableBase`
{
    fn len(&self) -> &isize {
        &self.as_ref().len
    }

    fn kind(&self) -> &VariableType {
        &self.as_ref().kind
    }

    fn struct_name(&self) -> &Option<String> {
        &self.as_ref().struct_name
    }
}

impl AsRef<VariableBase> for GlobalVar {
    fn as_ref(&self) -> &VariableBase {
        &self.base
    }
}

impl AsRef<VariableBase> for LocalVar {
    fn as_ref(&self) -> &VariableBase {
        &self.base
    }
}

impl LocalVar {
    pub fn create_int(index: i32) -> Self {
        LocalVar {
            base: VariableBase {
                kind: VariableType::Int,
                len: 0,
                struct_name: None,
            },
            num: index,
            name: String::new(),
        }
    }

    pub fn create_struct(index: i32, name: String) -> Self {
        LocalVar {
            base: VariableBase {
                kind: VariableType::Int,
                len: 0,
                struct_name: Some(name),
            },
            num: index,
            name: String::new(),
        }
    }
}

// An abstraction over local and global variable.
pub enum Operand {
    Local(LocalVar),
    Global(GlobalVar),
    Interger(i32),
}

pub struct GeneratorStore {
    global_defs: IndexMap<String, GlobalDef>,
    global_vars: IndexMap<String, VarDef<GlobalVar>>,
    local_vars: IndexMap<String, VarDef<LocalVar>>,
    ret_types: IndexMap<String, FnType>,
    label_index: AtomicI32,
    vreg_index: AtomicI32,
}

impl GeneratorStore {
    fn new() -> Self {
        Self {
            global_defs: IndexMap::new(),
            global_vars: IndexMap::new(),
            local_vars: IndexMap::new(),
            ret_types: IndexMap::new(),
            label_index: AtomicI32::new(0),
            vreg_index: AtomicI32::new(100),
        }
    }

    fn get_next_label_index(&self) -> i32 {
        return self.label_index.fetch_add(1, Ordering::SeqCst);
    }

    fn get_next_vreg_index(&self) -> i32 {
        return self.vreg_index.fetch_add(1, Ordering::SeqCst);
    }
}

pub fn gen(prog: Box<ast::Program>) {
    let mut store = GeneratorStore::new();
    handle_global_defs(prog, &mut store);
}

fn handle_global_defs(prog: Box<ast::Program>, store: &mut GeneratorStore) {
    for elem in prog.elements.iter() {
        use ast::ProgramElementInner::*;

        match &elem.inner {
            VarDeclStmt(stmt) => {
                handle_global_var_decl_stmt(stmt, store);
            }
            StructDef(struct_def) => {
                handle_struct_def(struct_def, store);
            }
            FnDeclStmt(fn_decl) => {
                handle_fn_decl(fn_decl, store);
            }
            FnDef(fn_def) => {
                handle_fn_def(fn_def, store);
            }
            _ => {
                panic!("[Error] Unsupported program element: {:?}", elem);
            }
        }
    }
}

fn handle_global_var_decl_stmt(stmt: &Box<ast::VarDeclStmt>, store: &mut GeneratorStore) {
    let val_type = stmt.inner.get_type().as_ref();
    let id = stmt.inner.get_id();
    let name = BlockLabel::from_str(id);

    let (kind, len, struct_name, init) = match &stmt.inner {
        ast::VarDeclStmtInner::Decl(decl) => match val_type {
            Some(ast::Type {
                inner: ast::TypeInner::StructType(ref name),
                ..
            }) => {
                let (kind, len) = match &decl.inner {
                    ast::VarDeclInner::Array(array_decl) => {
                        (VariableType::StructPtr, array_decl.len)
                    }
                    _ => (VariableType::Struct, 0),
                };
                (kind, len, Some(name.to_string()), Vec::new())
            }
            _ => {
                let (kind, len) = match &decl.inner {
                    ast::VarDeclInner::Array(array_decl) => (VariableType::IntPtr, array_decl.len),
                    _ => (VariableType::Int, 0),
                };
                (kind, len, None, Vec::new())
            }
        },
        ast::VarDeclStmtInner::Def(def) => match val_type {
            Some(ast::Type {
                inner: ast::TypeInner::StructType(ref name),
                ..
            }) => {
                let (kind, len) = match &def.inner {
                    ast::VarDefInner::Array(array_def) => (VariableType::StructPtr, array_def.len),
                    _ => (VariableType::Struct, 0),
                };
                (kind, len, Some(name.to_string()), Vec::new())
            }
            _ => {
                let mut init = Vec::new();
                let (kind, len) = match &def.inner {
                    ast::VarDefInner::Array(array_def) => {
                        for val in array_def.vals.iter() {
                            init.push(handle_right_val_first(val, &store));
                        }
                        (VariableType::IntPtr, array_def.len)
                    }
                    ast::VarDefInner::Scalar(scalar) => {
                        init.push(handle_right_val_first(scalar.val.as_ref(), &store));
                        (VariableType::Int, 0)
                    }
                };
                (kind, len, None, init)
            }
        },
    };

    store.global_vars.insert(
        id.clone(),
        VarDef {
            name: id.to_string(),
            var: GlobalVar {
                base: VariableBase {
                    kind,
                    len,
                    struct_name,
                },
                name,
            },
            init,
        },
    );
}

fn handle_right_val_first(r: &ast::RightVal, store: &GeneratorStore) -> i32 {
    match &r.inner {
        ast::RightValInner::ArithExpr(expr) => handle_arith_expr_first(&expr),
        ast::RightValInner::BoolExpr(expr) => handle_bool_expr_first(&expr, store),
    }
}

fn handle_arith_expr_first(expr: &ast::ArithExpr) -> i32 {
    match &expr.inner {
        ast::ArithExprInner::ArithBiOpExpr(expr) => handle_arith_biop_expr_first(&expr),
        ast::ArithExprInner::ExprUnit(unit) => handle_expr_unit_first(&unit),
    }
}

fn handle_bool_expr_first(expr: &ast::BoolExpr, store: &GeneratorStore) -> i32 {
    match &expr.inner {
        ast::BoolExprInner::BoolBiOpExpr(expr) => handle_bool_biop_expr(&expr, store),
        ast::BoolExprInner::BoolUnit(unit) => handle_bool_unit(&unit, store),
    }
}

fn handle_arith_biop_expr_first(expr: &ast::ArithBiOpExpr) -> i32 {
    let left = handle_arith_expr_first(&expr.left);
    let right = handle_arith_expr_first(&expr.right);
    match &expr.op {
        ast::ArithBiOp::Add => left + right,
        ast::ArithBiOp::Sub => left - right,
        ast::ArithBiOp::Mul => left * right,
        ast::ArithBiOp::Div => left / right,
    }
}

fn handle_expr_unit_first(expr: &ast::ExprUnit) -> i32 {
    match &expr.inner {
        ast::ExprUnitInner::Num(num) => *num,
        ast::ExprUnitInner::ArithExpr(expr) => handle_arith_expr_first(&expr),
        ast::ExprUnitInner::ArithUExpr(expr) => handle_arith_uexpr_first(&expr),
        _ => panic!("[Error] Not supported expr unit."),
    }
}

fn handle_arith_uexpr_first(u: &ast::ArithUExpr) -> i32 {
    if u.op == ast::ArithUOp::Neg {
        -handle_expr_unit_first(&u.expr)
    } else {
        0
    }
}

fn handle_bool_biop_expr(expr: &ast::BoolBiOpExpr, store: &GeneratorStore) -> i32 {
    let left = handle_bool_expr_first(&expr.left, store) != 0;
    let right = handle_bool_expr_first(&expr.right, store) != 0;
    if expr.op == ast::BoolBiOp::And {
        (left && right) as i32
    } else {
        (left || right) as i32
    }
}

fn handle_bool_unit(unit: &ast::BoolUnit, store: &GeneratorStore) -> i32 {
    match &unit.inner {
        ast::BoolUnitInner::ComExpr(expr) => handle_com_op_expr(&expr, store),
        ast::BoolUnitInner::BoolExpr(expr) => handle_bool_expr_first(&expr, store),
        ast::BoolUnitInner::BoolUOpExpr(expr) => handle_bool_uop_expr(&expr, store),
    }
}

fn handle_com_op_expr(expr: &ast::ComExpr, store: &GeneratorStore) -> i32 {
    let left = ptr_deref(handle_expr_unit(&expr.left, store));
    let right = ptr_deref(handle_expr_unit(&expr.right, store));
    match &expr.op {
        ast::ComOp::Lt => (left < right) as i32,
        ast::ComOp::Le => (left <= right) as i32,
        ast::ComOp::Gt => (left > right) as i32,
        ast::ComOp::Ge => (left >= right) as i32,
        ast::ComOp::Eq => (left == right) as i32,
        ast::ComOp::Ne => (left != right) as i32,
    }
}

fn handle_bool_uop_expr(expr: &ast::BoolUOpExpr, store: &GeneratorStore) -> i32 {
    if expr.op == ast::BoolUOp::Not {
        (handle_bool_unit(&expr.cond, store) == 0) as i32
    } else {
        0
    }
}

fn handle_expr_unit(unit: &ast::ExprUnit, store: &GeneratorStore) -> Operand {
    match &unit.inner {
        ast::ExprUnitInner::Num(num) => Operand::Interger(*num),
        ast::ExprUnitInner::Id(id) => {
            if let Some(def) = store.local_vars.get(id) {
                Operand::Local(def.var)
            } else if let Some(def) = store.global_vars.get(id) {
                Operand::Global(def.var)
            } else {
                panic!("[Error] {} undefined.", id);
            }
        }
        ast::ExprUnitInner::ArithExpr(expr) => handle_arith_expr(expr),
        ast::ExprUnitInner::FnCall(fn_call) => {
            let name = fn_call.name;
            let res = if let Some(t) = store.ret_types.get(&name) {
                match store.ret_types[&name].ret_type {
                    RetType::Int => {
                        Operand::Local(LocalVar::create_int(store.get_next_vreg_index()))
                    }
                    RetType::Struct => {
                        let type_name = store.ret_types[&name].struct_name;
                        Operand::Local(LocalVar::create_struct(
                            store.get_next_vreg_index(),
                            type_name,
                        ))
                    }
                    RetType::Void => {
                        panic!("[Error] invalid expr unit");
                    }
                }
            } else {
                panic!("[Error] {} undefined.", name);
            };

            // TODO

            res
        }
    }
}
