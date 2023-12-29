import { writeFileSync, unlinkSync } from "node:fs";
import { exec } from "child_process";
import { tests } from './tests.json' assert { type: 'json' };

const TESTS_FP = "spec/generated-tests";

writeFileSync(TESTS_FP, tests.map(({ code, input }) => `:k! TF.Core.RunTF "${code}" "${input}"`).join('\n'));

exec(`echo ":script ${TESTS_FP}" | cabal repl app -v0`, (error, stdout, stderr) => {
  if (error || stderr) {
    console.log(`error: ${error.message}`, `stderr: ${stderr}`);
    return;
  }

  const rawResults = stdout.split('\n').filter(s => s.startsWith('=')).map(s => s.slice(3, -1));

  const [messages, successes] = zip(tests, rawResults)
    .map(([test, result]) => ({
      ...test,
      result,
    }))
    .map(({ code, input, expect, result, prop }) => {
      const success = expect === result;

      const message = [
        `Ran code '${code}'`,
        (input) ? `with input '${input}'` : `with no input`,
        `to test ${prop}`,
        ` - Expect: '${expect}'`,
        ` - Actual: '${result}'`,
        (success) ? 'Success!' : 'Failure...',
      ].join('\n')

      return [message, success];
    })
    .unzip();

  const numOkay = successes.filter(x => x).length;
  const numFail = successes.length;

  const failures = zip(tests, successes)
    .filter(([_, success]) => !success)
    .map(([{ prop }]) => (
      ` - failed ${prop}`
    ))
    .join('\n');

  const output = [
    messages.join('\n\n'),
    '\n',
    '\n',
    `Results: ${numOkay}/${numFail}`,
    failures && '\n',
    failures
  ].join('');

  console.log(output);
  
  unlinkSync(TESTS_FP);
});

const zip = (a, b) => a.map((k, i) => [k, b[i]]);

Array.prototype.unzip = function() {
  return this.reduceRight(([as, bs], [a, b]) => [[a, ...as], [b, ...bs]], [[], []]);
}

export {}
