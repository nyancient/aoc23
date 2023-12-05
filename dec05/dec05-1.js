const parseSeeds = (lines, startLine) => {
    const words = lines[startLine].split(' ');
    if (words[0] !== "seeds:") {
        throw "oh no";
    }
    return {
        next: startLine + 1,
        value: words.slice(1).map(Number),
    }
};

const parseMap = (type, lines, startLine) => {
    if (lines[startLine] !== `${type} map:`) {
        throw "oh no";
    }

    const entries = [];
    let next = startLine + 1;
    for (; next < lines.length; next++) {
        const words = lines[next].split(' ');
        if (!words[0].match(/[0-9]+/)) {
            break;
        }
        entries.push({
            destRangeStart: Number(words[0]),
            sourceRangeStart: Number(words[1]),
            rangeLength: Number(words[2]),
        });
    }
    return {
        next: next,
        value: entries,
    };
};

const parse = (buf) => {
    const lines = buf.split('\n').filter(ln => ln);
    const seeds = parseSeeds(lines, 0);
    const seedToSoil = parseMap("seed-to-soil", lines, seeds.next);
    const soilToFertilizer = parseMap("soil-to-fertilizer", lines, seedToSoil.next);
    const fertilizerToWater = parseMap("fertilizer-to-water", lines, soilToFertilizer.next);
    const waterToLight = parseMap("water-to-light", lines, fertilizerToWater.next);
    const lightToTemp = parseMap("light-to-temperature", lines, waterToLight.next);
    const tempToHumidity = parseMap("temperature-to-humidity", lines, lightToTemp.next);
    const humidityToLoc = parseMap("humidity-to-location", lines, tempToHumidity.next);
    return {
        seeds: seeds.value,
        tables: [
            seedToSoil.value,
            soilToFertilizer.value,
            fertilizerToWater.value,
            waterToLight.value,
            lightToTemp.value,
            tempToHumidity.value,
            humidityToLoc.value,
        ],
    }
};

const seedToLoc = (seed, tables) => {
    let value = seed;
    for (const table of tables) {
        for (const entry of table) {
            const offset = value - entry.sourceRangeStart;
            if (offset >= 0 && offset < entry.rangeLength) {
                value = entry.destRangeStart + offset;
                break;
            }
        }
    }
    return value;
};

const main = async () => {
    let buf = "";
    for await (const bytes of process.stdin) {
        buf += bytes.toString();
    }
    const {seeds, tables} = parse(buf);
    console.log(Math.min(...seeds.map(s => seedToLoc(s, tables))));
};
main();