import * as elmCompiler from "https://deno.land/x/deno_elm_compiler@0.1.0/compiler.ts";

const [ inputFile, elmName, partToSolve ] = Deno.args;
const compiledElm = await elmCompiler.compileToString(`./src/${elmName}.elm`);

const globalEval = eval;
globalEval(compiledElm);;
const decoder = new TextDecoder();
const inputBuffer = await Deno.readFile(inputFile);
const inputText = await decoder.decode(inputBuffer);
// @ts-ignore
const app = globalThis.Elm[elmName].init({
  flags: inputText
});

app.ports.response.subscribe(function (result: string) {
  console.log(result);
});
app.ports[`part${partToSolve}`].send("");