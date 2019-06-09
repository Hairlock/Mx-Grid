const elmServerless = require('elm-serverless');
const consumption = require('./consumption.json');

// Import the elm app
const elm = require('./Main.elm');

module.exports.handler = elmServerless.httpApi({
    handler: elm.Api.Main,
    requestPort: 'requestPort',
    responsePort: 'responsePort',
    interop: {
        fetchHomeFlowRates: () => {
            return consumption.slice(0, 1000);
        }
    }
});
