/**
 * Unfinished Version of Day 1, Part 2
 */
class State {
    constructor() {
        this.value = 0
        this.pastStates = [];
    }

    apply(...strs) {
        for (const str of strs) {
            const { operator, number } = this._parse(str);
            switch (operator) {
                case "+":
                    this._add(number)
                    break;
                case "-":
                    this._sub(number)
                    break;
            }

            this.pastStates.push(this.value);
        }

        return this;
    }

    findFirstDuplicateInPastStates() {

    }

    _add(num) {
        this.value += num;
    }

    _sub(num) {
        this.value -= num;
    }

    _parse(str) {
        const [operator, ...tail] = str;

        return {
            operator,
            number: Number(tail.join(""))
        }
    }

}
