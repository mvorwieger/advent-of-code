class State {
    constructor() {
        this._value = 0
    }

    /**
     * @example ```
     *  const state = new State()
     *  state.apply("+23")
     *  state.apply(...["+23", "-20"])
     *  state.apply("+23").apply("-20")
     * ```
     * @param  {...any} strs
     */
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
        }

        return this;
    }

    _add(num) {
        this._value += num;
    }

    _sub(num) {
        this._value -= num;
    }

    _parse(str) {
        const [operator, ...tail] = str;

        return {
            operator,
            number: Number(tail.join(""))
        }
    }
}
