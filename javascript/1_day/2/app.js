/**
 * Object Oriented Way of Solving the day one (Part 1) Problem
 * to solve the problem:
 * ```
 * const arrayOfStateMutations = ["+23", "-21", "+5"]
 * const state = new State()
 * const state.apply(...arrayOfStateMutations) 
 * console.log(state.value)
 */
class State {
    constructor() {
        this.value = 0
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
