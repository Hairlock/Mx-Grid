const Sequelize = require('sequelize')
const mysql2 = require('mysql2')
const ExcessFlowModel = require('./ExcessFlow')

const sequelize = new Sequelize(
    process.env.DB_NAME,
    process.env.DB_USER,
    process.env.DB_PASSWORD,
    {
        dialect: 'mysql',
        dialectModule: mysql2,
        host: process.env.DB_HOST,
        port: process.env.DB_PORT
    }
)

const ExcessFlow = ExcessFlowModel(sequelize, Sequelize)
const Models = { ExcessFlow }
const connection = {}

module.exports = async () => {
    if (connection.isConnected) {
        return Models
    }

    await sequelize.sync()
    await sequelize.authenticate()
    connection.isConnected = true
    return Models
}