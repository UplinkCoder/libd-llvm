import std.stdio;
import std.string;
import std.json;


void main() {
	import std.file;
	if (exists("dub.json")) {
		File dubJson = File("dub.json","wra");
		writeln(dubJson.readln);
	}
	getLibString().writeLibs();
}

string getLibString(string p_llvmConfig="llvm-config") {
	import std.process;
	string llvmConfig = escapeShellFileName(p_llvmConfig);
	return std.process.executeShell(llvmConfig ~ " --libs")[1];
}

void writeLibs(string libString, File outs=stdout) {
	outs.write(`"libs" : `);
	foreach (libs;libString.split("-l").split(" ")) {
		outs.write ("[");
		foreach(lib;libs) {
			if (lib.chop != "") outs.write('"',lib.chop,'"',',');
		}
	outs.write(`"z","pthread","ncurses","stdc++","dl"`);
	outs.writeln("],");
	}
}
