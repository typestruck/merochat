import { ChangeEvent } from 'react'

export type input_source = HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement

export type change<S> = (field: keyof S) => (event: ChangeEvent<input_source>) => void;

export type nestedChange<S, N> = (field: keyof S, nestedField: keyof N) => (event: ChangeEvent<input_source>) => void;

export type Omit<T, K> = Pick<T, Exclude<keyof T, K>>;

export type Subtract<T, K> = Omit<T, keyof K>;

export interface User {
    id: string;
    name : string;
    avatar: string;
    birthday : Date;
    gender : string;
    country : number;
    description: string;
    tags : string [];
    recentEmoji? : string [];
    messageOnEnter? : boolean;
    headline: string;
    languages : number [];
}


