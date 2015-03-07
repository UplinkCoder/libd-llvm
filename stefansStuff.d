module stefansStuff;

Expression[] getExpressions(Statement s) {
	if (auto exprStmt = cast(ExpressionStatement)s) {
		return getExpressions(exprStmt.expression);
	} else if (auto returnStmt = cast(ReturnStatement)s) {
		return getExpressions(returnStmt.value);
	} else {
		assert(0);
	}
}

Expression[] getExpressions(Expression e) {
	return [e];
}

Statement[] getStatements(Function f) {
	return getStatements(f.fbody);
}

Statement[] getStatements(Statement stmt) {
	Statement[] result;
	
	if (auto blkStmt = cast(BlockStatement) stmt) {
		foreach(_stmt;blkStmt.statements) {
			result ~= getStatements(_stmt);
		}
		return result;
	}
	else { 
		return [stmt];
	}
}

if (Function calledfun = (cast(FunctionExpression) ((cast (CallExpression) e).callee)).fun)
foreach(Exprs;getStatements(calledfun).map!(s => getExpressions(s))) {
	foreach (callExpr;Exprs.filter!(e => cast(CallExpression)e)) {		
		writeln(callExpr.toString(codeGen.context));
	}
}