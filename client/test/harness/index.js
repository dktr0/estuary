const puppeteer = require('puppeteer');

;(async () => {
  const browser = await puppeteer.launch({args: ['--no-sandbox']});
  const page = await browser.newPage();

  await page.evaluateOnNewDocument(() => {
    window['ESTUARY_TEST_HARNESS_ENABLED'] = true
    window['ESTUARY_TEST_HARNESS_RESULTS'] = new Promise((resolve, reject) => {
      window['ESTUARY_TEST_HARNESS_RESULTS_CB'] = resolve;
    });
  });

  const url = 'http://localhost:8002';
  console.log(`Visiting ${url}...`);
  page.on('console', msg => console.log(`${url} ${msg.type().toUpperCase()}: ${msg.text()}`));

  await page.goto(url);

  const results = await page.evaluate(async () => await window['ESTUARY_TEST_HARNESS_RESULTS']);
  console.log('TEST RESULTS - ', results.success ? 'SUCCESS' : 'FAILURE');
  console.log(results.report);

  const performanceTiming = JSON.parse(
    await page.evaluate(() => JSON.stringify(window.performance.timing))
  );
  //console.log(performanceTiming);

  await browser.close();
  process.exitCode = results.success ? 0 : 1;
})();