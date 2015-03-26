module d.llvm.evaluator;
import d.context;
import d.llvm.codegen;
import d.ir.type;
import d.ir.expression;

import d.semantic.evaluator;

import util.visitor;

import llvm.c.analysis;
import llvm.c.core;
import llvm.c.executionEngine;

import std.algorithm;
import std.array;

// In order to JIT.
extern(C) void _d_assert(string, int);
extern(C) void _d_assert_msg(string, string, int);
extern(C) void _d_arraybounds(string, int);
extern(C) void* _d_allocmemory(size_t);

bool isString(Type t) {
	if (t.hasElement) {
		auto et = t.element.getCanonical();
		if (et.kind == TypeKind.Builtin && et.builtin == BuiltinType.Char) {
			return true;
		}
	}

	return false;
}


final class LLVMEvaluator : Evaluator {
	private CodeGenPass codeGen;
	
	
	this(CodeGenPass codeGen) {
		this.codeGen = codeGen;
	}
	
	CompileTimeExpression evaluate(Expression e) {
		if (auto ce = cast(CompileTimeExpression) e) {
			return ce;
		}

		return this.dispatch!(e => jit(e))(e);
	}
	
	CompileTimeExpression visit(TupleExpression e) {
		return new CompileTimeTupleExpression(e.location, e.type, e.values.map!(e => evaluate(e)).array());
	}

    auto createExecutionEngine(LLVMModuleRef dmodule)
    {
        char* errorPtr;

        LLVMExecutionEngineRef executionEngine;

        auto creationError = LLVMCreateMCJITCompilerForModule(&executionEngine, dmodule,  null, 0,  &errorPtr);
        if(creationError) {
            scope(exit) LLVMDisposeMessage(errorPtr);
            
            import std.c.string;
            auto error = errorPtr[0 .. strlen(errorPtr)].idup;

            import std.stdio;
            writeln(error);
            assert(0, "Cannot create execution engine ! Exiting...");
        }

        return executionEngine;
    }

	// Actual JIT
	private CompileTimeExpression jit(Expression e) {
		auto t = e.type.getCanonical();
		
		import d.ir.type;
		if (t.kind == TypeKind.Enum) {
			t = t.denum.type;
		}
		
		if (t.kind == TypeKind.Builtin) {
			auto k = t.builtin;
			if (isIntegral(k)) {
				auto returned = evalIntegral(e);
				
				return isSigned(k)
					? new IntegerLiteral!true(e.location, returned, k)
					: new IntegerLiteral!false(e.location, returned, k);
			} else if (k == BuiltinType.Bool) {
				return new BooleanLiteral(e.location, !!evalIntegral(e));
			}
		}
		
		if (t.kind == TypeKind.Slice) {
			auto et = t.element.getCanonical();
			if (et.kind == TypeKind.Builtin && et.builtin == BuiltinType.Char) {
				return new StringLiteral(e.location, evalString(e));
			}
		}
		
		assert(0, "Only able to JIT integers, booleans and strings, " ~ t.toString(codeGen.context) ~ " given.");
	}

    ulong evalIntegral(Expression e) {
        // When execution engine is made, it has a reference to dmodule.
        // It doesn't make sense to be passing this in here, and adding code to dmodule
        // FIXME(Shammah): Figure out what to do about this.

        scope(failure) LLVMDumpModule(codeGen.dmodule);
        
        auto funType = LLVMFunctionType(codeGen.visit(e.type), null, 0, false);
        
        auto fun = LLVMAddFunction(codeGen.dmodule, "__ctfe", funType);
        scope(exit) LLVMDeleteFunction(fun);
        
        auto backupCurrentBB = LLVMGetInsertBlock(codeGen.builder);
        scope(exit) {
            if(backupCurrentBB) {
                LLVMPositionBuilderAtEnd(codeGen.builder, backupCurrentBB);
            } else {
                LLVMClearInsertionPosition(codeGen.builder);
            }
        }
        
        auto bodyBB = LLVMAppendBasicBlockInContext(codeGen.llvmCtx, fun, "");
        LLVMPositionBuilderAtEnd(codeGen.builder, bodyBB);
        
        // Generate function's body.
        import d.llvm.expression;
        auto eg = ExpressionGen(codeGen);
        LLVMBuildRet(codeGen.builder, eg.visit(e));
        codeGen.checkModule(codeGen.dmodule);

		auto jitModule = /*LLVMCloneModule(*/ codeGen.dmodule;// );
		//scope(exit) LLVMDisposeModule(jitModule);
		auto executionEngine = createExecutionEngine(jitModule);
		//scope(exit) LLVMDisposeExecutionEngine(executionEngine);
        
		auto jitFun = LLVMGetNamedFunction(jitModule, "__ctfe");

        auto result = LLVMRunFunction(executionEngine, jitFun, 0, null);
        scope(exit) LLVMDisposeGenericValue(result);
		import std.stdio;
		//writeln("Dumping Module");
		//LLVMDumpModule(codeGen.dmodule);

		writeln(LLVMGenericValueToPointer(result));
        return LLVMGenericValueToInt(result, true);
    }
    
    string evalString(Expression e) in {
        // FIXME: newtype
		assert(isString(e.type), "this only CTFE strings.");
    } body {
		import std.c.stdlib;
		void* scratchpad = malloc(512);
		import d.semantic.type;
		import std.stdio;
		writeln(typeid(e),(cast(CallExpression)e).callee.type.toString(codeGen.context));
		codeGen.inJit = true;
		scope (exit) codeGen.inJit = false;
		auto llvm_memref = LLVMCreateGenericValueOfPointer(scratchpad); 	
		//auto recv = LLVMAddGlobal(codeGen.dmodule, LLVMPointerType(codeGen.visit(Type.get(BuiltinType.Void)), 0), "_scratchpad");
        // Create a global variable that recieve the string.
		auto stringTypePtr = LLVMPointerType(codeGen.visit(e.type),0);


		auto funType = LLVMFunctionType(codeGen.visit(Type.get(BuiltinType.Ulong)), null, 0, false);

		//auto funType = LLVMFunctionType(LLVMPointer(codeGen.visit(Type.get(BuiltinType.Char)), 0), null, 0, false);
        
        auto fun = LLVMAddFunction(codeGen.dmodule, "__ctfe", funType);
        scope(exit) LLVMDeleteFunction(fun);

//		scope(exit) LLVMViewFunctionCFG(fun);
        
        auto backupCurrentBB = LLVMGetInsertBlock(codeGen.builder);
        scope(exit) {
            if(backupCurrentBB) {
                LLVMPositionBuilderAtEnd(codeGen.builder, backupCurrentBB);
            } else {
                LLVMClearInsertionPosition(codeGen.builder);
            }
        }
        
        auto bodyBB = LLVMAppendBasicBlockInContext(codeGen.llvmCtx, fun, "");
        LLVMPositionBuilderAtEnd(codeGen.builder, bodyBB);
        
        // Generate function's body.
        import d.llvm.expression;
        auto eg = ExpressionGen(codeGen);
		auto expr = eg.visit(e);
		auto alloc = LLVMBuildAlloca(codeGen.builder, codeGen.visit(e.type), "");
		auto str = LLVMBuildStore(codeGen.builder, expr, alloc);
		auto len = LLVMBuildStructGEP(codeGen.builder, alloc, 0, "");
		auto ptr = LLVMBuildStructGEP(codeGen.builder, alloc, 1, "");
		//LLVMBuildStore(codeGen.builder, expr, );
		//auto ret = LLVMBuildAlloca(codeGen.builder, codeGen.visit(Type.get(BuiltinType.Ulong)),"");
		auto ret = LLVMBuildLoad(codeGen.builder, len, "");
		LLVMBuildRet(codeGen.builder, ret);
		LLVMDumpModule(codeGen.dmodule);
        codeGen.checkModule(codeGen.dmodule);
		import std.stdio;

		auto jitModule = /*LLVMCloneModule(*/ codeGen.dmodule;// );
		//scope(exit) LLVMDisposeModule(jitModule);
		//scope(failure) LLVMDumpModule(jitModule);
		auto executionEngine = createExecutionEngine(jitModule);
		//scope(exit) LLVMDisposeExecutionEngine(executionEngine);

		//auto jitFun = LLVMGetNamedFunction(jitModule, "__ctfe");
		writeln(typeid(expr));
		//LLVMDumpValue(jitFun);
		//auto jitReceiver = LLVMGetNamedGlobal(jitModule, "__ctString");
		//auto glob = LLVMGetPointerToGlobal(executionEngine, receiver);

		//LLVMDumpValue(receiver);

		string s;
		//LLVMAddGlobalMapping(executionEngine, receiver, &s);
		auto result = LLVMRunFunction(executionEngine, fun, 1, &llvm_memref);
		scope(exit) LLVMDisposeGenericValue(result);
		//LLVMDumpGenericValue(result);
		writeln(LLVMGenericValueToInt(result, false));
		writeln("\"",s, "\"");
        return "____";
    }
}

