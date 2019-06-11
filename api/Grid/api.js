const elmServerless = require('elm-serverless')
const { Grid } = require('./Api.elm')
const { Pool, Client } = require('pg')

const pool = new Pool({
    user: 'postgres',
    host: 'localhost',
    database: 'moixa',
    password: 'password',
    port: 5432
})

function getFlows() {
    return pool.query('SELECT * FROM excess', (err, res) => {
        if (err)
            console.log(err)
        pool.end()
        console.log(res.rows)
        return res.rows
    })}


let rates = require('./excess-rates').rates;

module.exports.handler = elmServerless.httpApi({
        handler: Grid.Api,
        interop: {
            getFlows : () => rates,
            addFlow : (f) => {
                return f;
            }
        }
    })