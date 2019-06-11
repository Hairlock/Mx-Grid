module.exports = (sequelize, type) => {
    return sequelize.define('excess', {
        id: {
            type: type.INTEGER,
            primaryKey: true,
            autoIncrement: true
        },
        flow: type.STRING,
        date: type.STRING
    })
}