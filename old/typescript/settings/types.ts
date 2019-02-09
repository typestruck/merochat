import { User, input_source } from '../types'
import {ChangeEvent, ReactNode} from 'react'

export enum Tab { Profile, Settings}

export interface ProfileProps {
    user: User;
    visible : boolean;
}

export interface ProfileState {
    user: User;
    currentTag : string;
}

export interface SettingsProps {
    user: User;
    visible : boolean;
}

export interface SettingsState {
    password : string;
    passwordConfirmation : string;
    email : string;
    emailConfirmation : string;
}

export interface SettingsProfileProps {
    user: User;
}

export interface SettingsProfileState{
    visible : Tab;
}

export interface CommonEditableProps {
    field: keyof User;
    defaultValue?: () => string;
}

export interface EditableProps extends CommonEditableProps {
    setHidden: () => void;
}

export interface InternalEditableProps extends CommonEditableProps {
    tag?: string;
    label: string;
    extraClasses?: string;
    user: User;
    display?: (v) => string;
}

export interface InputEditableProps extends EditableProps {
    enterCallback?: () => void;
    nodeBellow?: ReactNode;
    placeholder?: string;
    changeUserField: (key: keyof User) => (event: ChangeEvent<input_source>) => void;
}

export interface SelectEditableProps extends EditableProps {
    keepEditing?: boolean;//    
    children?: ReactNode;
    options: ReactNode;//
    changeUserField: (key: keyof User, enterCallback?: () => void) => (event: ChangeEvent<input_source>) => void;
}

export interface EditableState {
    displayClass: '' | 'hidden'
}
