use crate::{
    expr::{self, Expr},
    stmt::{self, Stmt},
};

pub struct AstPrinter {
    depth: i32,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self { depth: 0 }
    }
}

impl expr::Visitor<String, ()> for AstPrinter {
    fn binary(&mut self, expr: &expr::Binary) -> Result<String, ()> {
        Ok(self.parenthesize(expr.op.kind.as_string(), &[&expr.left, &expr.right]))
    }

    fn grouping(&mut self, expr: &expr::Grouping) -> Result<String, ()> {
        Ok(self.parenthesize("group".to_string(), &[&expr.expr]))
    }

    fn literal(&mut self, expr: &expr::Literal) -> Result<String, ()> {
        Ok(format!("{}", expr.lit))
    }

    fn unary(&mut self, expr: &expr::Unary) -> Result<String, ()> {
        Ok(self.parenthesize(expr.op.kind.as_string(), &[&expr.expr]))
    }

    fn var(&mut self, expr: &expr::Var) -> Result<String, ()> {
        Ok(self.parenthesize(expr.name.kind.as_string(), &[]))
    }

    fn assignment(&mut self, expr: &expr::Assignment) -> Result<String, ()> {
        Ok(self.parenthesize(
            "ASSIGN ".to_owned() + &expr.name.kind.as_string(),
            &[&expr.value],
        ))
    }

    fn logical(&mut self, expr: &expr::Logical) -> Result<String, ()> {
        Ok(self.parenthesize(expr.op.kind.as_string(), &[&expr.left, &expr.right]))
    }

    fn call(&mut self, expr: &expr::Call) -> Result<String, ()> {
        let args = &expr.args.iter().collect::<Vec<_>>();
        let callee = expr.callee.accept(self).unwrap();
        Ok(self.parenthesize(callee, args))
    }

    fn lambda(&mut self, expr: &expr::Lambda) -> Result<String, ()> {
        let mut str = String::from("(FN");

        str += " (";
        for (i, arg) in expr.params.iter().enumerate() {
            str += &arg.kind.as_string();

            if i + 1 < expr.params.len() {
                str += " ";
            }
        }

        str += ")\n    (";

        self.depth += 1;

        for stmt in &expr.body {
            str += &stmt.accept(self).unwrap();
            str += "\n";
            for _ in 0..self.depth {
                str += "    "
            }
        }

        self.depth -= 1;

        str += "\n";
        for _ in 0..self.depth-1 {
            str += "    "
        }
        str += ")";

        Ok(str)
    }
}

impl stmt::Visitor<String, ()> for AstPrinter {
    fn print(&mut self, stmt: &stmt::Print) -> Result<String, ()> {
        Ok(self.parenthesize("PRINT".to_string(), &[&stmt.expr]))
    }

    fn vardecl(&mut self, stmt: &stmt::VarDecl) -> Result<String, ()> {
        let name = "VAR ".to_owned() + &stmt.name.kind.as_string();
        let init = &stmt.initializer;
        Ok(self.parenthesize(name, &[init]))
    }

    fn expr(&mut self, stmt: &stmt::Expression) -> Result<String, ()> {
        Ok(self.parenthesize("".to_string(), &[&stmt.expr]))
    }

    fn block(&mut self, stmt: &stmt::Block) -> Result<String, ()> {
        let mut str = String::from("(BLOCK\n");
        self.depth += 1;

        for stmt in stmt.statements.as_slice() {
            for _ in 0..self.depth {
                str += "    ";
            }
            str += stmt.accept(self).unwrap().as_str();
            str += "\n";
        }

        for _ in 0..self.depth - 1 {
            str += "    ";
        }
        str += ")";

        self.depth -= 1;
        Ok(str)
    }

    fn if_stmt(&mut self, stmt: &stmt::If) -> Result<String, ()> {
        let mut str = String::from("(IF ");
        self.depth += 1;

        str += &stmt.condition.accept(self).unwrap();
        str += "\n";
        for _ in 0..self.depth {
            str += "    ";
        }
        str += &stmt.then_branch.accept(self).unwrap();
        if let Some(else_b) = &stmt.else_branch {
            str += "\n";
            for _ in 0..self.depth {
                str += "    ";
            }
            str += &else_b.accept(self).unwrap();
        }
        str += "\n";
        for _ in 0..self.depth - 1 {
            str += "    ";
        }
        str += ")";

        self.depth -= 1;

        Ok(str)
    }

    fn while_stmt(&mut self, stmt: &stmt::While) -> Result<String, ()> {
        let mut str = String::from("(WHILE ");
        self.depth += 1;

        str += &stmt.condition.accept(self).unwrap();
        str += "\n";
        for _ in 0..self.depth {
            str += "    ";
        }
        str += &stmt.statement.accept(self).unwrap();

        str += "\n";
        for _ in 0..self.depth - 1 {
            str += "    ";
        }
        str += ")";

        self.depth -= 1;

        Ok(str)
    }

    fn function(&mut self, stmt: &stmt::Function) -> Result<String, ()> {
        let mut str = String::from("(FN ");
        str += &stmt.name.kind.as_string();

        str += " (";
        for (i, arg) in stmt.params.iter().enumerate() {
            str += &arg.kind.as_string();

            if i + 1 < stmt.params.len() {
                str += " ";
            }
        }

        str += ")\n    (";

        self.depth += 1;
        for stmt in &stmt.body {
            str += &stmt.accept(self).unwrap();
            str += "\n";
            for _ in 0..self.depth {
                str += "    "
            }
        }

        self.depth -= 1;

        str += "\n";
        for _ in 0..self.depth-1 {
            str += "    "
        }
        str += ")";

        Ok(str)
    }

    fn return_stmt(&mut self, stmt: &stmt::Return) -> Result<String, ()> {
        let mut str = String::from("(");
        str += &stmt.keyword.kind.as_string().to_uppercase();
        str += " ";
        if let Some(ret_val) = &stmt.value {
            str += &ret_val.accept(self).unwrap();
        }
        str += ")";

        Ok(str)
    }

    fn break_stmt(&mut self, _stmt: &stmt::Break) -> Result<String, ()> {
        Ok(self.parenthesize(String::from("BREAK"), &[]))
    }
}

impl AstPrinter {
    pub fn print(&mut self, stmts: &Vec<Stmt>) -> String {
        let mut str = "".to_string();

        for stmt in stmts {
            str += stmt.accept(self).unwrap().as_str();
            str += "\n";
        }

        str
    }

    fn parenthesize(&mut self, name: String, exprs: &[&Expr]) -> String {
        let mut str = "(".to_string();
        str += &name;
        for expr in exprs {
            str += " ";

            str += expr.accept(self).unwrap().as_str();
        }
        str += ")";

        str
    }
}
