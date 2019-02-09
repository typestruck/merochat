import { ChangeEvent } from 'react'
import { input_source } from './types'

/** Helps with the boilerplate of handling input events. */
export function changer<State>(field: keyof State): (event: ChangeEvent<input_source>) => void {
    return event => {
        this.setState({ [field]: event.target.type == "checkbox" || event.target.type == "radio" ? (event.target as HTMLInputElement).checked : event.target.value });
    };
}

/** Helps with the boilerplate of handling input events. */
export function nestedChanger<State, Nested>(field: keyof State, nestedField: keyof Nested): (event: ChangeEvent<input_source>) => void {
    return event => {
        if (Array.isArray(this.state[field][nestedField])) {
            let newArray = this.state[field][nestedField].slice();

            newArray.push(event.target.value);

            this.setState({ [field]: { ...this.state[field], [nestedField]: newArray } });
        }
        else
            this.setState({ [field]: { ...this.state[field], [nestedField]: event.target.value } });

    };
}
