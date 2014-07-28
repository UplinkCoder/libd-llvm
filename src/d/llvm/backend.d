module d.llvm.backend;

import d.llvm.codegen;
import d.llvm.evaluator;

import d.ir.symbol;

import d.context;

import llvm.c.core;
import llvm.c.executionEngine;
import llvm.c.target;
import llvm.c.targetMachine;

import llvm.c.transforms.passManagerBuilder;

import std.array;
import std.process;
import std.stdio;
import std.string;

final class LLVMBackend {
	private CodeGenPass pass;
	private LLVMExecutionEngineRef executionEngine;
	private LLVMEvaluator evaluator;
	
	private uint optLevel;
	private string linkerParams;
	private uint bitWidth;

	this(Context context, string name, uint optLevel, string linkerParams, uint bitWidth) {
		LLVMInitializeX86TargetInfo();
		LLVMInitializeX86Target();
		LLVMInitializeX86TargetMC();
		
		LLVMLinkInJIT();
		LLVMInitializeX86AsmPrinter();
		
		this.optLevel = optLevel;
		this.linkerParams = linkerParams;
		this.bitWidth = bitWidth;
		
		pass = new CodeGenPass(context, name, bitWidth);
		
		char* errorPtr;
		auto creationError = LLVMCreateJITCompilerForModule(&executionEngine, pass.dmodule, 0, &errorPtr);
		if(creationError) {
			scope(exit) LLVMDisposeMessage(errorPtr);
			
			import std.c.string;
			auto error = errorPtr[0 .. strlen(errorPtr)].idup;
			
			writeln(error);
			assert(0, "Cannot create execution engine ! Exiting...");
		}
		
		evaluator = new LLVMEvaluator(executionEngine, pass);
	}
	
	auto getPass() {
		return pass;
	}
	
	auto getEvaluator() {
		return evaluator;
	}
	
	void visit(Module mod) {
		pass.visit(mod);
	}
	
	void visit(Function f) {
		pass.visit(f);
	}
	
	void emitObject(Module[] modules, string objFile) {
		foreach(m; modules) {
			visit(m);
		}
		
		auto dmodule = pass.dmodule;
		
		auto pmb = LLVMPassManagerBuilderCreate();
		scope(exit) LLVMPassManagerBuilderDispose(pmb);
		
		if(optLevel == 0) {
			LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, 0);
			LLVMPassManagerBuilderSetOptLevel(pmb, 0);
		} else {
			LLVMDumpModule(dmodule);
			writeln("\n; Optimized as :");
			
			LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, 100);
			LLVMPassManagerBuilderSetOptLevel(pmb, optLevel);
		}
		
		auto pm = LLVMCreatePassManager();
		scope(exit) LLVMDisposePassManager(pm);
		
		LLVMAddTargetData(LLVMGetExecutionEngineTargetData(executionEngine), pm);
		LLVMPassManagerBuilderPopulateModulePassManager(pmb, pm);
		
		LLVMRunPassManager(pm, dmodule);
		
		// Dump module for debug purpose.
		LLVMDumpModule(dmodule);
		
		version(OSX) {
			auto triple = "x86_64-apple-darwin9".ptr;
		} version (linux) {
			auto triple = "x86_64-pc-linux-gnu".ptr;
		}
		LLVMTargetMachineRef targetMachine;
		switch (bitWidth) {
			case 32 : 
				targetMachine = LLVMCreateTargetMachine(LLVMGetNextTarget(LLVMGetFirstTarget()), triple, "i386".ptr, "".ptr, LLVMCodeGenOptLevel.Default, LLVMRelocMode.Default, LLVMCodeModel.Default);
			break;
			case 64 : 
				targetMachine = LLVMCreateTargetMachine(LLVMGetFirstTarget(), triple, "x86-64".ptr, "".ptr, LLVMCodeGenOptLevel.Default, LLVMRelocMode.Default, LLVMCodeModel.Default);
			break;
			default : assert(0,"unspecifyed bitWidth");
		}
		
		scope(exit) LLVMDisposeTargetMachine(targetMachine);
		
		/*
		writeln("\nASM generated :");
		
		LLVMTargetMachineEmitToFile(targetMachine, dmodule, "/dev/stdout".ptr, LLVMCodeGenFileType.Assembly, &errorPtr);
		//*/
		/+
		version(linux) {
			// Hack around the need of _tlsstart and _tlsend.
			auto _tlsstart = LLVMAddGlobal(dmodule, LLVMInt32Type(), "_tlsstart");
			LLVMSetInitializer(_tlsstart, LLVMConstInt(LLVMInt32Type(), 0, true));
			LLVMSetSection(_tlsstart, ".tdata");
			LLVMSetLinkage(_tlsstart, LLVMLinkage.LinkOnceODR);
			
			auto _tlsend = LLVMAddGlobal(dmodule, LLVMInt32Type(), "_tlsend");
			LLVMSetInitializer(_tlsend, LLVMConstInt(LLVMInt32Type(), 0, true));
			LLVMSetThreadLocal(_tlsend, true);
			LLVMSetLinkage(_tlsend, LLVMLinkage.LinkOnceODR);
		}
		// +/
		char* errorPtr;
		auto linkError = LLVMTargetMachineEmitToFile(targetMachine, dmodule, toStringz(objFile), LLVMCodeGenFileType.Object, &errorPtr);
		if(linkError) {
			scope(exit) LLVMDisposeMessage(errorPtr);
			
			import std.c.string;
			writeln(errorPtr[0 .. strlen(errorPtr)]);
			
			assert(0, "Fail to link ! Exiting...");
		}
	}
	
	void link(string objFile, string executable) {
		string stdlib = "sdrt";
                if (bitWidth==32) stdlib ~= "32";

		auto linkCommand = "gcc -o " ~ escapeShellFileName(executable) ~ " " ~ escapeShellFileName(objFile) ~ linkerParams ~ " -l"~stdlib;
		
		writeln(linkCommand);
		wait(spawnShell(linkCommand));
	}
}

