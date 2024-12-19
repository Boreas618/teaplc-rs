use super::{BiOpKind, BlockLabel, Operand, RelOpKind};
use std::rc::Rc;

pub enum StmtInner {
    Call(CallStmt),
    Load(LoadStmt),
    BiOp(BiOpStmt),
    Alloca(AllocaStmt),
    Cmp(CmpStmt),
    CJump(CJumpStmt),
    Label(LabelStmt),
    Store(StoreStmt),
    Jump(JumpStmt),
    Gep(GepStmt),
}

pub struct Stmt {
    inner: StmtInner,
}

impl Stmt {
    pub fn as_call(func_name: String, res: Rc<Operand>, args: Vec<Rc<Operand>>) -> Self {
        Self {
            inner: StmtInner::Call(CallStmt {
                func_name,
                res,
                args,
            }),
        }
    }

    pub fn as_load(dst: Rc<Operand>, ptr: Rc<Operand>) -> Self {
        Self {
            inner: StmtInner::Load(LoadStmt { dst, ptr }),
        }
    }

    pub fn as_biop(
        kind: BiOpKind,
        left: Rc<Operand>,
        right: Rc<Operand>,
        dst: Rc<Operand>,
    ) -> Self {
        Self {
            inner: StmtInner::BiOp(BiOpStmt {
                kind,
                left,
                right,
                dst,
            }),
        }
    }

    pub fn as_alloca(dst: Rc<Operand>) -> Self {
        Self {
            inner: StmtInner::Alloca(AllocaStmt { dst }),
        }
    }

    pub fn as_cmp(
        kind: RelOpKind,
        left: Rc<Operand>,
        right: Rc<Operand>,
        dst: Rc<Operand>,
    ) -> Self {
        Self {
            inner: StmtInner::Cmp(CmpStmt {
                kind,
                left,
                right,
                dst,
            }),
        }
    }

    pub fn as_cjump(dst: Rc<Operand>, true_label: BlockLabel, false_label: BlockLabel) -> Self {
        Self {
            inner: StmtInner::CJump(CJumpStmt {
                dst,
                true_label,
                false_label,
            }),
        }
    }

    pub fn as_label(label: BlockLabel) -> Self {
        Self {
            inner: StmtInner::Label(LabelStmt { label }),
        }
    }

    pub fn as_store(src: Rc<Operand>, ptr: Rc<Operand>) -> Self {
        Self {
            inner: StmtInner::Store(StoreStmt { src, ptr }),
        }
    }

    pub fn as_jump(target: BlockLabel) -> Self {
        Self {
            inner: StmtInner::Jump(JumpStmt { target }),
        }
    }

    pub fn as_gep(new_ptr: Rc<Operand>, base_ptr: Rc<Operand>, index: Rc<Operand>) -> Self {
        Self {
            inner: StmtInner::Gep(GepStmt {
                new_ptr,
                base_ptr,
                index,
            }),
        }
    }
}

pub struct CallStmt {
    func_name: String,
    res: Rc<Operand>,
    args: Vec<Rc<Operand>>,
}

pub struct LoadStmt {
    dst: Rc<Operand>,
    ptr: Rc<Operand>,
}

pub struct BiOpStmt {
    kind: BiOpKind,
    left: Rc<Operand>,
    right: Rc<Operand>,
    dst: Rc<Operand>,
}

pub struct AllocaStmt {
    dst: Rc<Operand>,
}

pub struct CmpStmt {
    kind: RelOpKind,
    left: Rc<Operand>,
    right: Rc<Operand>,
    dst: Rc<Operand>,
}

pub struct CJumpStmt {
    dst: Rc<Operand>,
    true_label: BlockLabel,
    false_label: BlockLabel,
}

pub struct LabelStmt {
    label: BlockLabel,
}

pub struct StoreStmt {
    src: Rc<Operand>,
    ptr: Rc<Operand>,
}

pub struct JumpStmt {
    target: BlockLabel,
}

pub struct GepStmt {
    new_ptr: Rc<Operand>,
    base_ptr: Rc<Operand>,
    index: Rc<Operand>,
}
