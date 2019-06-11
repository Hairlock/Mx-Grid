const elmServerless = require('elm-serverless');
const consumption = require('./consumption.json');

// Import the elm app
const elm = require('./Main.elm');

module.exports.handler = elmServerless.httpApi({
    handler: elm.Api.Main,
    requestPort: 'requestPort',
    responsePort: 'responsePort',
    interop: {
        fetchHomeFlowRates: ({ page, limit }) => {
            return consumption.slice(page * limit, (page + 1) * limit);
        }
    }
});
