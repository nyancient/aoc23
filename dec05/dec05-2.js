const parseSeeds = (lines, startLine) => {
    const words = lines[startLine].split(' ');
    if (words[0] !== "seeds:") {
        throw "oh no";
    }
    const seeds = [];
    for (let i = 1; i < words.length; i += 2) {
        seeds.push({start: Number(words[i]), length: Number(words[i+1])});
    }
    return {
        next: startLine + 1,
        value: seeds,
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

const intersectLines = (a, b) => {
    const a_end = a.start + a.length - 1;
    const b_end = b.start + b.length - 1;
    if (a.start >= b.start && a_end <= b_end) {
        return a;
    }
    if (b.start >= a.start && b_end <= a_end) {
        return b;
    }
    if (a.start < b.start && a_end >= b.start) {
        return {
            start: b.start,
            length: a_end - b.start,
        }
    }
    if (b.start < a.start && b_end >= a.start) {
        return {
            start: a.start,
            length: b_end - a.start,
        }
    }
    return undefined;
};

const seedToLoc = (seed, tables) => {
    let value = seed;
    for (const table of tables) {
        for (const entry of table) {
            const sourceLine = {
                start: entry.sourceRangeStart,
                length: entry.rangeLength,
            };
            const intersection = intersectLines(value, sourceLine);
            if (intersection) {
                value.start = intersection.start + (entry.destRangeStart - entry.sourceRangeStart);
                value.length = intersection.length;
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
    console.log(Math.min(...seeds.map(s => seedToLoc(s, tables).start)));
};
main();