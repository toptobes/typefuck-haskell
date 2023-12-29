import { writeFileSync, unlinkSync } from "node:fs";
import { exec } from "child_process";
import { tests } from './tests.json' assert { type: 'json' };

const TEST_FP = "spec/generated-tests";

writeFileSync(TEST_FP, tests.map(({ code, input }) => `:k! TF.Core.RunTF "${code}" "${input}"`).join('\n'));

exec(`echo ":script ${TEST_FP}" | cabal repl app -v0`, (error, stdout, stderr) => {
  if (error || stderr) {
    console.log(`error: ${error.message}`, `stderr: ${stderr}`);
    return;
  }

  const results = stdout.split('\n').filter(s => s.startsWith('=')).map(s => s.slice(3, -1));

  const zipped = zip(tests, results).map(([test, result]) => ({
    ...test,
    result,
  }));

  const output = zipped.map(({ code, input, expect, result, prop }) => [
    `Ran code '${code}'`,
    (input) ? `with input '${input}'` : `with no input`,
    `to test ${prop}`,
    ` - Expect: '${expect}'`,
    ` - Actual: '${result}'`,
    (expect === result) ? 'Success!' : 'Failure...',
  ].join('\n')).join('\n\n');

  console.log(output);
  
  unlinkSync(TEST_FP);
});

const zip = (a, b) => a.map((k, i) => [k, b[i]]);

export {}
