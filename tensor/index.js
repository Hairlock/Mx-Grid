import "babel-polyfill";

async function getData() {
    const homeRatesDataReq = await fetch('http://localhost:3000/homerates?page=0&limit=1000');
    const homeRatesData = await homeRatesDataReq.json();
    const cleaned = homeRatesData.map(rate => ({
        demand: rate.demand,
        solar: rate.solar,
    }))
        .filter(car => (car.demand != null && car.solar != null));

    return cleaned;
}



function convertToTensor(data) {
    // Wrapping these calculations in a tidy will dispose any 
    // intermediate tensors.

    return tf.tidy(() => {
        // Step 1. Shuffle the data    
        tf.util.shuffle(data);

        // Step 2. Convert data to Tensor
        const inputs = data.map(d => d.solar)
        const labels = data.map(d => d.demand);

        const inputTensor = tf.tensor2d(inputs, [inputs.length, 1]);
        const labelTensor = tf.tensor2d(labels, [labels.length, 1]);

        //Step 3. Normalize the data to the range 0 - 1 using min-max scaling
        const inputMax = inputTensor.max();
        const inputMin = inputTensor.min();
        const labelMax = labelTensor.max();
        const labelMin = labelTensor.min();

        const normalizedInputs = inputTensor.sub(inputMin).div(inputMax.sub(inputMin));
        const normalizedLabels = labelTensor.sub(labelMin).div(labelMax.sub(labelMin));

        return {
            inputs: normalizedInputs,
            labels: normalizedLabels,
            // Return the min/max bounds so we can use them later.
            inputMax,
            inputMin,
            labelMax,
            labelMin,
        }
    });
}

async function trainModel(model, inputs, labels) {
    // Prepare the model for training.  
    model.compile({
        optimizer: tf.train.adam(),
        loss: tf.losses.meanSquaredError,
        metrics: ['mse'],
    });

    const batchSize = 28;
    const epochs = 30;

    return await model.fit(inputs, labels, {
        batchSize,
        epochs,
        shuffle: true,
        callbacks: tfvis.show.fitCallbacks(
            { name: 'Training Performance' },
            ['loss', 'mse'],
            { height: 200, callbacks: ['onEpochEnd'] }
        )
    });
}

function createModel() {
    // Create a sequential model
    const model = tf.sequential();

    // Add a single hidden layer
    model.add(tf.layers.dense({ inputShape: [1], units: 1, useBias: true }));

    // Add an output layer
    model.add(tf.layers.dense({ units: 1, useBias: true }));

    return model;
}

function testModel(model, inputData, normalizationData) {
    const { inputMax, inputMin, labelMin, labelMax } = normalizationData;

    // Generate predictions for a uniform range of numbers between 0 and 1;
    // We un-normalize the data by doing the inverse of the min-max scaling 
    // that we did earlier.
    const [xs, preds] = tf.tidy(() => {

        const xs = tf.linspace(0, 1, 100);
        const preds = model.predict(xs.reshape([100, 1]));

        const unNormXs = xs
            .mul(inputMax.sub(inputMin))
            .add(inputMin);

        const unNormPreds = preds
            .mul(labelMax.sub(labelMin))
            .add(labelMin);

        // Un-normalize the data
        return [unNormXs.dataSync(), unNormPreds.dataSync()];
    });


    const predictedPoints = Array.from(xs).map((val, i) => {
        return { x: val, y: preds[i] }
    });

    const originalPoints = inputData.map(d => ({
        x: d.solar, y: d.demand,
    }));


    tfvis.render.scatterplot(
        { name: 'Model Predictions vs Original Data' },
        { values: [originalPoints, predictedPoints], series: ['original', 'predicted'] },
        {
            xLabel: 'solar',
            yLabel: 'demand',
            height: 300
        }
    );
}


async function trainModel(model, inputs, labels) {
    // Prepare the model for training.  
    model.compile({
        optimizer: tf.train.adam(),
        loss: tf.losses.meanSquaredError,
        metrics: ['mse'],
    });

    const batchSize = 28;
    const epochs = 20;

    return await model.fit(inputs, labels, {
        batchSize,
        epochs,
        shuffle: true,
        callbacks: tfvis.show.fitCallbacks(
            { name: 'Training Performance' },
            ['loss', 'mse'],
            { height: 200, callbacks: ['onEpochEnd'] }
        )
    });
}

async function run() {
    // Load and plot the original input data that we are going to train on.
    const data = await getData();
    const values = data.map(d => ({
        x: d.solar,
        y: d.demand,
    }));

    tfvis.render.scatterplot(
        { name: 'solar v demand' },
        { values },
        {
            xLabel: 'solar',
            yLabel: 'demand',
            height: 300
        }
    );

    // More code will be added below
    const tensorData = convertToTensor(data);
    const { inputs, labels } = tensorData;

    const model = createModel();
    tfvis.show.modelSummary({ name: 'Model Summary' }, model);

    // Train the model  
    await trainModel(model, inputs, labels);
    console.log('Done Training');
    testModel(model, data, tensorData);
}

document.addEventListener('DOMContentLoaded', run);