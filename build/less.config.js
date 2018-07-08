const fs = require('fs-extra');
const less = require('less');
const LessPluginAutoPrefix = require('less-plugin-autoprefix');
const LessPluginCleanCSS = require('less-plugin-clean-css');
const browsers = require('./browserlist');
const autoPrefixPlugin = new LessPluginAutoPrefix({
  browsers,
});
const cleanCSSPlugin = new LessPluginCleanCSS({
  advanced: true,
});

async function convert(inputFilename, outputFilename) {
  try {
    const data = await fs.readFile(inputFilename, {
      encoding: 'utf8',
    });
    const output = await less.render(data, {
      plugins: [autoPrefixPlugin, cleanCSSPlugin],
      filename: inputFilename,
    });
    await fs.writeFile(outputFilename, output.css);
    console.log(`[${new Date()}]: ${outputFilename} has been saved.`);
  } catch ( e ) {
    console.error('ERROR:', e);
  }
}

exports.generateCss = () => convert('./css/style.less', './dist/style.min.css');


