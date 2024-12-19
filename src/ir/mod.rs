mod stmt;

use crate::ast::{self, BoolBiOp};
use core::panic;
use indexmap::IndexMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicI32, Ordering};

pub enum VariableType {
    Int,
    IntPtr,
    Struct,
    StructPtr,
}

#[derive(Clone)]
pub enum RetType {
    Int,
    Struct,
    Void,
}

pub enum BiOpKind {
    Plus,
    Minus,
    Mul,
    Div,
}

pub enum RelOpKind {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
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

#[derive(Clone)]
pub struct FnType {
    ret_type: RetType,
    struct_name: String,
}

pub struct FnDecl {
    name: String,
    args: Vec<VarDef>,
    ret: FnType,
}

pub struct VarDef {
    name: String,
    var: Rc<Operand>,
    init: Vec<i32>,
}

pub enum GlobalDef {
    Struct(StructDef),
    Func(FnDecl),
}

#[derive(Clone)]
pub struct BlockLabel {
    name: String,
}

impl BlockLabel {
    fn from_index(index: i32) -> Self {
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

    pub fn create_int_ptr(index: i32, len: isize) -> Self {
        LocalVar {
            base: VariableBase {
                kind: VariableType::IntPtr,
                len,
                struct_name: None,
            },
            num: index,
            name: String::new(),
        }
    }

    pub fn create_struct(index: i32, name: String) -> Self {
        LocalVar {
            base: VariableBase {
                kind: VariableType::Struct,
                len: 0,
                struct_name: Some(name),
            },
            num: index,
            name: String::new(),
        }
    }

    pub fn create_struct_ptr(index: i32, len: isize, name: String) -> Self {
        LocalVar {
            base: VariableBase {
                kind: VariableType::StructPtr,
                len,
                struct_name: Some(name),
            },
            num: index,
            name: String::new(),
        }
    }
}

// An abstraction over local and global variable.
pub enum Operand {
    Local(Box<dyn Variable>),
    Global(Box<dyn Variable>),
    Interger(i32),
}

pub struct MemberProp {
    offset: i32,
    def: VarDef,
}

pub struct StructProp {
    member_props: IndexMap<String, MemberProp>,
}

pub struct FnProp {
    name: String,
    ret: RetType,
    args: Vec<Rc<Operand>>,
    irs: Vec<stmt::Stmt>,
}

pub struct GeneratorStore {
    global_defs: IndexMap<String, GlobalDef>,
    global_vars: IndexMap<String, VarDef>,
    local_vars: IndexMap<String, VarDef>,
    ret_types: IndexMap<String, FnType>,
    label_index: AtomicI32,
    vreg_index: AtomicI32,
    emit_irs: Vec<stmt::Stmt>,
    struct_props: IndexMap<String, StructProp>,
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
            emit_irs: Vec::new(),
            struct_props: IndexMap::new(),
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
    handle_global_defs(&prog, &mut store);
}

fn handle_global_defs(prog: &Box<ast::Program>, store: &mut GeneratorStore) {
    for elem in prog.elements.iter() {
        use ast::ProgramElementInner::*;

        match &elem.inner {
            VarDeclStmt(stmt) => {
                handle_global_var_decl_stmt(stmt, store);
            }
            StructDef(_) => {
                // handle_struct_def(struct_def, store);
                todo!()
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
                            init.push(handle_right_val_first(val, store));
                        }
                        (VariableType::IntPtr, array_def.len)
                    }
                    ast::VarDefInner::Scalar(scalar) => {
                        init.push(handle_right_val_first(scalar.val.as_ref(), store));
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
            var: Rc::new(Operand::Global(Box::new(GlobalVar {
                base: VariableBase {
                    kind,
                    len,
                    struct_name,
                },
                name,
            }))),
            init,
        },
    );
}

fn handle_fn_decl(stmt: &Box<ast::FnDeclStmt>, store: &mut GeneratorStore) {
    let ft = match stmt.fn_decl.ret_type.as_ref() {
        None => FnType {
            ret_type: RetType::Void,
            struct_name: String::new(),
        },
        Some(t) => match &t.inner {
            ast::TypeInner::NativeType(_) => FnType {
                ret_type: RetType::Int,
                struct_name: String::new(),
            },
            ast::TypeInner::StructType(type_name) => FnType {
                ret_type: RetType::Struct,
                struct_name: type_name.as_ref().clone(),
            },
        },
    };

    let id = stmt.fn_decl.id.clone();
    if store.ret_types.get(&id).is_none() {
        store.ret_types.insert(id.clone(), ft.clone());
    }

    let mut args: Vec<VarDef> = Vec::new();
    match stmt.fn_decl.param_decl.as_ref() {
        None => (),
        Some(params) => {
            for decl in params.decls.iter() {
                match &decl.inner {
                    ast::VarDeclInner::Scalar(_) => match decl.real_type.as_ref() {
                        None => args.push(VarDef {
                            name: String::new(),
                            var: Rc::new(Operand::Local(Box::new(LocalVar::create_int(
                                store.get_next_vreg_index(),
                            )))),
                            init: Vec::new(),
                        }),
                        Some(inner) => match &inner.inner {
                            ast::TypeInner::StructType(t) => {
                                args.push(VarDef {
                                    name: t.as_ref().clone(),
                                    var: Rc::new(Operand::Local(Box::new(
                                        LocalVar::create_struct_ptr(
                                            store.get_next_vreg_index(),
                                            0,
                                            t.as_ref().clone(),
                                        ),
                                    ))),
                                    init: Vec::new(),
                                });
                            }
                            ast::TypeInner::NativeType(_) => {
                                args.push(VarDef {
                                    name: String::new(),
                                    var: Rc::new(Operand::Local(Box::new(LocalVar::create_int(
                                        store.get_next_vreg_index(),
                                    )))),
                                    init: Vec::new(),
                                });
                            }
                        },
                    },
                    ast::VarDeclInner::Array(_) => match decl.real_type.as_ref() {
                        None => args.push(VarDef {
                            name: String::new(),
                            var: Rc::new(Operand::Local(Box::new(LocalVar::create_int(
                                store.get_next_vreg_index(),
                            )))),
                            init: Vec::new(),
                        }),
                        Some(inner) => match &inner.inner {
                            ast::TypeInner::StructType(t) => {
                                args.push(VarDef {
                                    name: t.as_ref().clone(),
                                    var: Rc::new(Operand::Local(Box::new(
                                        LocalVar::create_struct_ptr(
                                            store.get_next_vreg_index(),
                                            -1,
                                            t.as_ref().clone(),
                                        ),
                                    ))),
                                    init: Vec::new(),
                                });
                            }
                            ast::TypeInner::NativeType(_) => {
                                args.push(VarDef {
                                    name: String::new(),
                                    var: Rc::new(Operand::Local(Box::new(
                                        LocalVar::create_int_ptr(store.get_next_vreg_index(), -1),
                                    ))),
                                    init: Vec::new(),
                                });
                            }
                        },
                    },
                }
            }
        }
    }

    store.global_defs.insert(
        id.clone(),
        GlobalDef::Func(FnDecl {
            name: id,
            args: args,
            ret: ft.clone(),
        }),
    );
}

fn handle_fn_def(stmt: &Box<ast::FnDef>, store: &mut GeneratorStore) {
    let id = stmt.fn_decl.id.clone();
    if store.ret_types.get(&id).is_none() {
        let ft = match stmt.fn_decl.ret_type.as_ref() {
            None => FnType {
                ret_type: RetType::Void,
                struct_name: String::new(),
            },
            Some(t) => match &t.inner {
                ast::TypeInner::NativeType(_) => FnType {
                    ret_type: RetType::Int,
                    struct_name: String::new(),
                },
                ast::TypeInner::StructType(type_name) => FnType {
                    ret_type: RetType::Struct,
                    struct_name: type_name.as_ref().clone(),
                },
            },
        };

        store.ret_types.insert(id, ft);
    }
}

fn handle_right_val_first(r: &ast::RightVal, store: &mut GeneratorStore) -> i32 {
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

fn handle_bool_expr_first(expr: &ast::BoolExpr, store: &mut GeneratorStore) -> i32 {
    match &expr.inner {
        ast::BoolExprInner::BoolBiOpExpr(expr) => handle_bool_biop_expr_first(&expr, store),
        ast::BoolExprInner::BoolUnit(unit) => handle_bool_unit_first(&unit, store),
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

fn handle_bool_biop_expr_first(expr: &ast::BoolBiOpExpr, store: &mut GeneratorStore) -> i32 {
    let left = handle_bool_expr_first(&expr.left, store) != 0;
    let right = handle_bool_expr_first(&expr.right, store) != 0;
    if expr.op == ast::BoolBiOp::And {
        (left && right) as i32
    } else {
        (left || right) as i32
    }
}

fn handle_bool_unit_first(unit: &ast::BoolUnit, store: &mut GeneratorStore) -> i32 {
    match &unit.inner {
        ast::BoolUnitInner::ComExpr(expr) => handle_com_op_expr(&expr, store),
        ast::BoolUnitInner::BoolExpr(expr) => handle_bool_expr_first(&expr, store),
        ast::BoolUnitInner::BoolUOpExpr(expr) => handle_bool_uop_expr(&expr, store),
    }
}

fn handle_com_op_expr(expr: &ast::ComExpr, store: &mut GeneratorStore) -> i32 {
    let _ = ptr_deref(handle_expr_unit(&expr.left, store), store).as_ref();
    let _ = ptr_deref(handle_expr_unit(&expr.right, store), store).as_ref();
    /*match &expr.op {
        ast::ComOp::Lt => (left < right) as i32,
        ast::ComOp::Le => (left <= right) as i32,
        ast::ComOp::Gt => (left > right) as i32,
        ast::ComOp::Ge => (left >= right) as i32,
        ast::ComOp::Eq => (left == right) as i32,
        ast::ComOp::Ne => (left != right) as i32,
    }*/
    return 0;
}

fn handle_bool_uop_expr(expr: &ast::BoolUOpExpr, store: &mut GeneratorStore) -> i32 {
    if expr.op == ast::BoolUOp::Not {
        (handle_bool_unit_first(&expr.cond, store) == 0) as i32
    } else {
        0
    }
}

fn handle_expr_unit(unit: &ast::ExprUnit, store: &mut GeneratorStore) -> Rc<Operand> {
    match &unit.inner {
        ast::ExprUnitInner::Num(num) => Rc::new(Operand::Interger(*num)),
        ast::ExprUnitInner::Id(id) => {
            if let Some(def) = store.local_vars.get(id) {
                Rc::clone(&def.var)
            } else if let Some(def) = store.global_vars.get(id) {
                Rc::clone(&def.var)
            } else {
                panic!("[Error] {} undefined.", id);
            }
        }
        ast::ExprUnitInner::ArithExpr(expr) => handle_arith_expr(expr, store),
        ast::ExprUnitInner::FnCall(fn_call) => {
            let name = fn_call.name.clone();
            let res = if let Some(_) = store.ret_types.get(&name) {
                match store.ret_types[&name].ret_type {
                    RetType::Int => {
                        Operand::Local(Box::new(LocalVar::create_int(store.get_next_vreg_index())))
                    }
                    RetType::Struct => {
                        let type_name = store.ret_types[&name].struct_name.clone();
                        Operand::Local(Box::new(LocalVar::create_struct(
                            store.get_next_vreg_index(),
                            type_name,
                        )))
                    }
                    RetType::Void => {
                        panic!("[Error] invalid expr unit");
                    }
                }
            } else {
                panic!("[Error] {} undefined.", name);
            };
            let res_op = Rc::new(res);

            let mut args: Vec<Rc<Operand>> = Vec::new();
            for arg in fn_call.vals.as_ref().unwrap().iter() {
                args.push(ptr_deref(handle_right_val(&arg, store), store));
            }
            store
                .emit_irs
                .push(stmt::Stmt::as_call(name, Rc::clone(&res_op), args));

            res_op
        }
        ast::ExprUnitInner::ArrayExpr(expr) => handle_array_expr(&expr, store),
        ast::ExprUnitInner::MemberExpr(expr) => handle_member_expr(&expr, store),
        ast::ExprUnitInner::ArithUExpr(expr) => handle_arith_uexpr(&expr, store),
    }
}

fn handle_arith_expr(expr: &ast::ArithExpr, store: &mut GeneratorStore) -> Rc<Operand> {
    match &expr.inner {
        ast::ArithExprInner::ArithBiOpExpr(expr) => handle_arith_biop_expr(&expr, store),
        ast::ArithExprInner::ExprUnit(unit) => handle_expr_unit(&unit, store),
    }
}

fn handle_arith_biop_expr(expr: &ast::ArithBiOpExpr, store: &mut GeneratorStore) -> Rc<Operand> {
    let left = handle_arith_expr(&expr.left, store);
    let right = handle_arith_expr(&expr.right, store);
    let dst = Rc::new(Operand::Local(Box::new(LocalVar::create_int(
        store.get_next_vreg_index(),
    ))));

    let kind = match expr.op {
        ast::ArithBiOp::Add => BiOpKind::Plus,
        ast::ArithBiOp::Sub => BiOpKind::Minus,
        ast::ArithBiOp::Mul => BiOpKind::Mul,
        ast::ArithBiOp::Div => BiOpKind::Div,
    };

    let biop_stmt = stmt::Stmt::as_biop(kind, left, right, Rc::clone(&dst));
    store.emit_irs.push(biop_stmt);

    dst
}

fn handle_right_val(val: &ast::RightVal, store: &mut GeneratorStore) -> Rc<Operand> {
    match &val.inner {
        ast::RightValInner::ArithExpr(expr) => handle_arith_expr(expr, store),
        ast::RightValInner::BoolExpr(expr) => {
            let true_label = BlockLabel::from_index(store.get_next_label_index());
            let false_label = BlockLabel::from_index(store.get_next_label_index());
            let after_label = BlockLabel::from_index(store.get_next_label_index());

            let bool_evaluated = LocalVar::create_int_ptr(store.get_next_vreg_index(), 0);

            let alloca_dst = Rc::new(Operand::Local(Box::new(bool_evaluated)));
            store
                .emit_irs
                .push(stmt::Stmt::as_alloca(Rc::clone(&alloca_dst)));
            handle_bool_expr(
                expr,
                Some(true_label.clone()),
                Some(false_label.clone()),
                store,
            );

            produce_bool_val(
                true_label.clone(),
                false_label.clone(),
                after_label,
                Rc::clone(&alloca_dst),
                store,
            );

            let bool_val = LocalVar::create_int(store.get_next_vreg_index());
            let dst = Rc::new(Operand::Local(Box::new(bool_val)));
            let ptr = Rc::clone(&alloca_dst);
            store
                .emit_irs
                .push(stmt::Stmt::as_load(Rc::clone(&dst), ptr));

            dst
        }
    }
}

fn handle_bool_expr(
    expr: &ast::BoolExpr,
    true_label: Option<BlockLabel>,
    false_label: Option<BlockLabel>,
    store: &mut GeneratorStore,
) -> Option<Rc<Operand>> {
    if true_label.is_none() && false_label.is_none() {
        let true_label = BlockLabel::from_index(store.get_next_label_index());
        let false_label = BlockLabel::from_index(store.get_next_label_index());
        let after_label = BlockLabel::from_index(store.get_next_label_index());

        let bool_evaluated = LocalVar::create_int_ptr(store.get_next_vreg_index(), 0);

        let alloca_dst = Rc::new(Operand::Local(Box::new(bool_evaluated)));
        store
            .emit_irs
            .push(stmt::Stmt::as_alloca(Rc::clone(&alloca_dst)));

        match &expr.inner {
            ast::BoolExprInner::BoolBiOpExpr(expr) => handle_bool_biop_expr(
                expr,
                Some(true_label.clone()),
                Some(false_label.clone()),
                store,
            ),
            ast::BoolExprInner::BoolUnit(unit) => handle_bool_unit(
                unit,
                Some(true_label.clone()),
                Some(false_label.clone()),
                store,
            ),
        }

        produce_bool_val(
            true_label,
            false_label,
            after_label,
            Rc::clone(&alloca_dst),
            store,
        );

        let truncated = LocalVar::create_int(store.get_next_vreg_index());
        let bool_val = LocalVar::create_int(store.get_next_vreg_index());

        let dst = Rc::new(Operand::Local(Box::new(bool_val)));
        let ptr = Rc::clone(&alloca_dst);
        let retval = Rc::new(Operand::Local(Box::new(truncated)));

        store
            .emit_irs
            .push(stmt::Stmt::as_load(Rc::clone(&dst), Rc::clone(&ptr)));

        store.emit_irs.push(stmt::Stmt::as_cmp(
            RelOpKind::Ne,
            Rc::clone(&dst),
            Rc::new(Operand::Interger(0)),
            Rc::clone(&retval),
        ));

        return Some(retval);
    } else if true_label.is_none() || false_label.is_none() {
        panic!("[Error] one of the jump target is null.");
    }

    match &expr.inner {
        ast::BoolExprInner::BoolBiOpExpr(expr) => {
            handle_bool_biop_expr(expr, true_label, false_label, store)
        }
        ast::BoolExprInner::BoolUnit(unit) => {
            handle_bool_unit(unit, true_label, false_label, store)
        }
    }

    return None;
}

fn handle_bool_biop_expr(
    expr: &ast::BoolBiOpExpr,
    true_label: Option<BlockLabel>,
    false_label: Option<BlockLabel>,
    store: &mut GeneratorStore,
) {
    let eval_right_label = BlockLabel::from_index(store.get_next_label_index());
    let lhs = handle_bool_expr(&expr.left, None, None, store);

    let false_label_unwrapped = false_label.unwrap().clone();
    let true_label_unwrapped = true_label.unwrap().clone();

    match &expr.op {
        BoolBiOp::And => {
            store.emit_irs.push(stmt::Stmt::as_cjump(
                lhs.unwrap(),
                eval_right_label.clone(),
                false_label_unwrapped.clone(),
            ));
            store.emit_irs.push(stmt::Stmt::as_label(eval_right_label));

            let rhs = handle_bool_expr(&expr.right, None, None, store);
            store.emit_irs.push(stmt::Stmt::as_cjump(
                rhs.unwrap(),
                true_label_unwrapped,
                false_label_unwrapped,
            ))
        }
        BoolBiOp::Or => {
            store.emit_irs.push(stmt::Stmt::as_cjump(
                lhs.unwrap(),
                true_label_unwrapped.clone(),
                eval_right_label.clone(),
            ));
            store.emit_irs.push(stmt::Stmt::as_label(eval_right_label));

            let rhs = handle_bool_expr(&expr.right, None, None, store);
            store.emit_irs.push(stmt::Stmt::as_cjump(
                rhs.unwrap(),
                true_label_unwrapped,
                false_label_unwrapped,
            ))
        }
    }
}

fn handle_bool_unit(
    unit: &ast::BoolUnit,
    true_label: Option<BlockLabel>,
    false_label: Option<BlockLabel>,
    store: &mut GeneratorStore,
) {
    match &unit.inner {
        ast::BoolUnitInner::ComExpr(expr) => {
            let _ = handle_com_op_expr(expr, store);
        }
        ast::BoolUnitInner::BoolExpr(expr) => {
            let _ = handle_bool_expr(expr, true_label, false_label, store);
        }
        ast::BoolUnitInner::BoolUOpExpr(expr) => {
            let _ = handle_bool_unit(&expr.cond, false_label, true_label, store);
        }
    }
}

fn produce_bool_val(
    true_label: BlockLabel,
    false_label: BlockLabel,
    after_label: BlockLabel,
    bool_evaluated: Rc<Operand>,
    store: &mut GeneratorStore,
) {
    let store_src_true = Rc::new(Operand::Interger(1));
    let store_src_false = Rc::new(Operand::Interger(0));
    let store_ptr = Rc::clone(&bool_evaluated);

    store.emit_irs.push(stmt::Stmt::as_label(true_label));
    store
        .emit_irs
        .push(stmt::Stmt::as_store(store_src_true, Rc::clone(&store_ptr)));
    store
        .emit_irs
        .push(stmt::Stmt::as_jump(after_label.clone()));

    store.emit_irs.push(stmt::Stmt::as_label(false_label));
    store
        .emit_irs
        .push(stmt::Stmt::as_store(store_src_false, Rc::clone(&store_ptr)));

    store.emit_irs.push(stmt::Stmt::as_jump(after_label));
}

fn handle_array_expr(expr: &ast::ArrayExpr, store: &mut GeneratorStore) -> Rc<Operand> {
    let arr = handle_left_val(&expr.arr, store);
    let target = match arr.as_ref() {
        Operand::Interger(_) => panic!("[Error] Invalid array expression"),
        Operand::Local(inner) | Operand::Global(inner) => {
            if let VariableType::IntPtr = inner.kind() {
                Rc::new(Operand::Local(Box::new(LocalVar::create_int_ptr(
                    store.get_next_vreg_index(),
                    0,
                ))))
            } else {
                let id = inner.struct_name();
                Rc::new(Operand::Local(Box::new(LocalVar::create_struct_ptr(
                    store.get_next_vreg_index(),
                    0,
                    id.as_deref().unwrap().to_string(),
                ))))
            }
        }
    };

    let index = handle_index_expr(expr.idx.as_ref(), store);

    store
        .emit_irs
        .push(stmt::Stmt::as_gep(Rc::clone(&target), arr, index));

    target
}

fn handle_left_val(val: &ast::LeftVal, store: &mut GeneratorStore) -> Rc<Operand> {
    match &val.inner {
        ast::LeftValInner::Id(id) => {
            let lval;
            if store.local_vars.get(id).is_some() {
                lval = Rc::clone(&store.local_vars.get(id).unwrap().var)
            } else if store.global_vars.get(id).is_some() {
                lval = Rc::clone(&store.global_vars.get(id).unwrap().var)
            } else {
                panic!("[Error] {} not found.", &id);
            }
            lval
        }
        ast::LeftValInner::ArrayExpr(expr) => handle_array_expr(expr, store),
        ast::LeftValInner::MemberExpr(expr) => handle_member_expr(expr, store),
    }
}

fn handle_member_expr(expr: &ast::MemberExpr, store: &mut GeneratorStore) -> Rc<Operand> {
    let s = handle_left_val(&expr.struct_id, store);
    let type_name = match s.as_ref() {
        Operand::Interger(_) => panic!("[Error]: Invalid Left value."),
        Operand::Local(inner) | Operand::Global(inner) => inner.struct_name(),
    }
    .clone()
    .unwrap();

    let member = store
        .struct_props
        .get(&type_name)
        .unwrap()
        .member_props
        .get(&expr.member_id)
        .unwrap();

    let target = match member.def.var.as_ref() {
        Operand::Interger(_) => panic!("Invalid member"),
        Operand::Local(inner) | Operand::Global(inner) => match inner.kind() {
            VariableType::Int => Rc::new(Operand::Local(Box::new(LocalVar::create_int_ptr(
                store.get_next_vreg_index(),
                0,
            )))),
            VariableType::IntPtr => Rc::new(Operand::Local(Box::new(LocalVar::create_int_ptr(
                store.get_next_vreg_index(),
                *inner.len(),
            )))),
            VariableType::Struct => Rc::new(Operand::Local(Box::new(LocalVar::create_struct_ptr(
                store.get_next_vreg_index(),
                0,
                inner.struct_name().clone().unwrap(),
            )))),
            VariableType::StructPtr => {
                Rc::new(Operand::Local(Box::new(LocalVar::create_struct_ptr(
                    store.get_next_vreg_index(),
                    *inner.len(),
                    inner.struct_name().clone().unwrap(),
                ))))
            }
        },
    };

    store.emit_irs.push(stmt::Stmt::as_gep(
        Rc::clone(&target),
        s,
        Rc::new(Operand::Interger(member.offset)),
    ));

    target
}

fn handle_index_expr(expr: &ast::IndexExpr, store: &mut GeneratorStore) -> Rc<Operand> {
    match &expr.inner {
        ast::IndexExprInner::Id(id) => {
            let idx = LocalVar::create_int(store.get_next_vreg_index());
            let retval = Rc::new(Operand::Local(Box::new(idx)));
            if store.local_vars.get(id).is_some() {
                let src = store.local_vars.get(id).unwrap();
                store
                    .emit_irs
                    .push(stmt::Stmt::as_load(Rc::clone(&retval), Rc::clone(&src.var)));
            } else if store.global_vars.get(id).is_some() {
                let src = store.global_vars.get(id).unwrap();
                store
                    .emit_irs
                    .push(stmt::Stmt::as_load(Rc::clone(&retval), Rc::clone(&src.var)));
            } else {
                panic!("[Errpr] {} undefined.", id);
            }

            retval
        }
        ast::IndexExprInner::Num(num) => Rc::new(Operand::Interger(*num as i32)),
    }
}

fn handle_arith_uexpr(expr: &ast::ArithUExpr, store: &mut GeneratorStore) -> Rc<Operand> {
    let val = ptr_deref(handle_expr_unit(expr.expr.as_ref(), store), store);
    let res = LocalVar::create_int(store.get_next_vreg_index());
    let res_op = Rc::new(Operand::Local(Box::new(res)));
    store.emit_irs.push(stmt::Stmt::as_biop(
        BiOpKind::Minus,
        Rc::new(Operand::Interger(0)),
        val,
        Rc::clone(&res_op),
    ));

    res_op
}

fn ptr_deref(op: Rc<Operand>, store: &mut GeneratorStore) -> Rc<Operand> {
    match op.as_ref() {
        Operand::Interger(_) => op,
        Operand::Local(inner) | Operand::Global(inner) => {
            if let VariableType::IntPtr = inner.kind() {
                if *inner.len() == 0 {
                    let val = LocalVar::create_int(store.get_next_vreg_index());
                    let dst = Rc::new(Operand::Local(Box::new(val)));
                    let stmt = stmt::Stmt::as_load(Rc::clone(&dst), op);
                    store.emit_irs.push(stmt);

                    return dst;
                } else {
                    return op;
                }
            } else if let VariableType::Int = inner.kind() {
                op
            } else {
                op
            }
        }
    }
}
