const fs = require('fs-extra');

async function getLineHeight() {
  try {
    const str = await fs.readFile('css/style.less', {
      encoding: 'utf8',
    });
    return parseInt(str.match(/@line-height:\s?(\d+)px;/)[1]);
  } catch ( e ) {
    console.warn('read line height failed: ' + e);
    return 21;
  }
}

async function base64Encode(file) {
  const image = await fs.readFile(file);
  return new Buffer(image).toString('base64');
}

async function generateHtml() {
  const placeholder = '<!-- inject index.js -->';
  const htmlfile = await fs.readFile('build/template.html', {
    encoding: 'utf8',
  });
  const height = await getLineHeight();
  const indexjs = await fs.readFile('build/index.js', {
    encoding: 'utf8',
  });
  const jsfile = `const lineHeight = ${height};
${indexjs}`;
  const favicon = await base64Encode('favicon.ico');
  await fs.writeFile('index.html',
    htmlfile
      .replace(placeholder, jsfile)
      .replace('href="favicon.ico"',
        `href="data:image/x-icon;base64,${favicon}"`)
  );
  console.log(`[${new Date().toJSON()}]: index.html has been saved`);
}

exports.generateHtml = generateHtml;
