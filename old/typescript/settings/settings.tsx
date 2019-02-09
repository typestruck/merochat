import * as melan from '../melanchat'
import React, { Component, Fragment, ChangeEvent, RefObject, FocusEvent, KeyboardEvent, ComponentType } from 'react'
import * as comp from '../component'
import ReactDOM from 'react-dom'
import { Subtract, change, nestedChange, User, input_source } from '../types'
import {EditableProps, EditableState, InternalEditableProps, InputEditableProps, SelectEditableProps, SettingsProps, SettingsState, ProfileProps, ProfileState, SettingsProfileProps, SettingsProfileState, Tab } from './types'

function editableWith<P extends EditableProps>(Wrapped: ComponentType<P>) {
    return class extends Component<Subtract<P, EditableProps> & InternalEditableProps, EditableState> {

        editingSentence = 'Click to edit';

        state: EditableState = { displayClass: 'hidden' };

        render() {
            let props = this.props as InternalEditableProps,
                Tag = props.tag || 'span';

            return (
                <>
                    <div tabIndex={1} className={`editing ${this.props.extraClasses} ${this.state.displayClass}`} onBlur={this.unedit()}>
                        <label> {this.props.label}</label>
                        <Wrapped setHidden={this.setHidden} defaultValue={this.defaultValue} {...this.props}></Wrapped>
                    </div>

                    <Tag className={`content-focus ${this.toggleDisplayClass()}`} onClick={this.edit} title={`${this.editingSentence} ${this.props.field}`}>{this.showFieldOrDefault()}</Tag>
                </>);
        }

        showFieldOrDefault() {
            let value = this.props.user[this.props.field];

            if (!value || Array.isArray(value) && value.length == 0)
                return `${this.editingSentence} ${this.props.field}`;

            if (this.props.display)
                return this.props.display(value);

            return `${value}`;
        }

        /** Makes sure the input is blurred only if it is clicked outside of the parent div. */
        unedit = () =>
            (event: FocusEvent<HTMLDivElement>) => {
                let currentTarget = event.currentTarget;

                setTimeout(() => {
                    if (!currentTarget.contains(document.activeElement))
                        this.setHidden();
                }, 0);
            }

        setHidden = () => {
            this.setState({ displayClass: 'hidden' });
        }

        edit = () => {
            this.setState({ displayClass: '' });
        }

        toggleDisplayClass() {
            return this.state.displayClass == 'hidden' ? '' : 'hidden';
        }

        defaultValue = () => `${(this.props.defaultValue ? this.props.defaultValue() : this.props.user[this.props.field]) || ''}`;
    }
}

class Input extends Component<InputEditableProps, {}> {

    input: RefObject<HTMLInputElement>  = React.createRef();

    componentDidUpdate() {
       this.input.current.focus();
      //  this.input.current.select();
    }

    render() {
        return (
            <>
                {this.props.children}
                <input type="text" placeholder={this.props.placeholder} ref={this.input} onKeyDown={this.uneditOnEnter()} onChange={this.props.changeUserField(this.props.field)} defaultValue={this.props.defaultValue()} />
                {this.props.nodeBellow}
            </>);
    }

    uneditOnEnter = () => {
        return (event: KeyboardEvent<input_source>) => {
            if (event.key == 'Enter') {
                event.preventDefault();

                if (this.props.enterCallback)
                    this.props.enterCallback();
                else
                    this.props.setHidden();
            }
        }
    }
}

function Select(props: SelectEditableProps) {
    return (
        <>
            <select onChange={props.changeUserField(props.field, props.keepEditing ? null : props.setHidden)} defaultValue={props.defaultValue()}><option value="">Don't show</option>
                {props.options}
            </select>
            {props.children}
        </>);
}

const InputEditable = editableWith(Input),
      SelectEditable = editableWith(Select);

/** User private configurations such as email or password. */
class Settings extends Component<SettingsProps, SettingsState> {

    state = {
        email: '',
        password: '',
        emailConfirmation: '',
        passwordConfirmation: ''
    };

    changer: change<SettingsState> = comp.changer.bind(this);

    render() {
        let classes = 'profile';

        if (!this.props.visible)
            classes = `${classes} hidden`;

        return (
            <div className={classes} >
                <div>
                    <label>New password</label>
                    <input type="password" onChange={this.changer('password')} />
                </div>
                <div>
                    <label>Confirm new password</label>
                    <input type="password" onChange={this.changer('passwordConfirmation')} />
                </div>
                <div>
                    <label>New email</label>
                    <input type="text" onChange={this.changer('email')} />
                </div>
                <div>
                    <label>Confirm new email</label>
                    <input type="text" onChange={this.changer('emailConfirmation')} />
                </div>
                <br />
                <div>
                    <input type="button" onClick={this.save} value="Update settings" />
                </div>
            </div>
        );
    }

    save = () => {
        let toSave = {} as SettingsState;

        if (this.state.email || this.state.emailConfirmation) {
            if (this.state.email != this.state.emailConfirmation)
                alert('Email and confirmation do not match');
            else if (!this.state.email || !this.state.email.includes('@'))
                alert('Email does not  seem to be valid');
            else
                toSave.email = this.state.email;
        }

        if (this.state.password || this.state.passwordConfirmation) {
            if (this.state.password == this.state.passwordConfirmation)
                toSave.password = this.state.password;
            else
                alert('Password and confirmation do not match')
        }

        if (Object.keys(toSave).length > 0)
            melan.post('/user/update/settings', toSave, r => {
                this.setState({ email: '', emailConfirmation: '', password: '', passwordConfirmation: '' });

                alert('Updated');
            });
    }
}

/** Public configurations such as display name or headline. */
class Profile extends Component<ProfileProps, ProfileState> {

    changeUserField: (key: keyof User, enterCallback?: () => void) => (event: ChangeEvent<input_source>) => void = (key, callback) => event => {
       let cc : nestedChange<ProfileState, User> = comp.nestedChanger.bind(this);

       cc('user', key)(event);

        if (callback)
            callback();
    };

    state = {
        user: {
            ...this.props.user,
            birthday: (this.props.user.birthday ? new Date(this.props.user.birthday) : undefined)
        },
        currentTag: ''
    };

    render() {
        let classes = 'profile',
            countryName = melan.countryName(this.state.user.country),
            languages = melan.languageNames(this.state.user.languages),
            days = [];

        if (melan.age(this.state.user.birthday) > 0) {
            let day = new Date(this.state.user.birthday.getFullYear(), this.state.user.birthday.getMonth(), 1).getDate(),
                lastDay = new Date(this.state.user.birthday.getFullYear(), this.state.user.birthday.getMonth() + 1, 0).getDate();

            while (day <= lastDay) {
                days.push(<option key={day} value={day}>{day}</option>);
                day++;
            }
        }

        if (!this.props.visible)
            classes = `${classes} hidden`;

        return (
            <div className={classes}>
                <div className="profile-info">
                    <div>
                        <img className="avatar-profile" src={this.props.user.avatar} />
                    </div>

                    <InputEditable field="name" tag="h1" label="Your display name. Leave blank for a new random name" changeUserField={this.changeUserField} user={this.state.user}></InputEditable>

                    <InputEditable field="headline" tag="h3" label="Your headline- a short message to catch someone's attention. Leave blank for a new random headline." changeUserField={this.changeUserField} user={this.state.user}></InputEditable>
                    <div>
                        <InputEditable field="birthday" extraClasses="inline" defaultValue={() => this.getBirthday(Date.prototype.getFullYear)} placeholder="year" tag="span" label="Your birthday, used to compute your age." display={d => `${melan.age(d)}`} changeUserField={() => this.setBirthday(Date.prototype.setFullYear)} user={this.state.user}>
                            <select onChange={this.setBirthday(Date.prototype.setMonth)} defaultValue={this.getBirthday(melan.getMonth1)}>
                                <option value="">Month</option>
                                <option value="0">January</option>
                                <option value="1">February</option>
                                <option value="2">March</option>
                                <option value="3">April</option>
                                <option value="4">May</option>
                                <option value="5">June</option>
                                <option value="6">July</option>
                                <option value="7">August</option>
                                <option value="8">September</option>
                                <option value="9">October</option>
                                <option value="10">November</option>
                                <option value="11">December</option>
                            </select>
                            <select onChange={this.setBirthday(Date.prototype.setDate)} defaultValue={this.getBirthday(Date.prototype.getDate)}>
                                <option value="">Day</option>
                                {days}
                            </select>
                        </InputEditable>

                        <SelectEditable field="gender" label="Your gender." changeUserField={this.changeUserField} user={this.state.user} options={<>
                            <option value="F">Female</option>
                            <option value="M">Male</option>
                            <option value="O">Other</option></>}></SelectEditable>

                        <SelectEditable field="country" label="Your country." changeUserField={this.changeUserField} user={this.state.user} options={melan.countryList.map(c => (<option key={c.id} value={c.id}>{c.name}</option>))} display={() => countryName} ></SelectEditable>

                        <SelectEditable field="languages" keepEditing={true} label="The languages other people can chat with you in(안녕하세요?)" changeUserField={this.changeUserField} user={this.state.user} options={melan.languageList.map(c => (<option key={c.id} value={c.id}>{c.name}</option>))} display={() =>
                            languages.length > 0 ?
                                languages.join(', ') : ''}>

                            {languages.map((l, i) => (<Fragment key={i}> <span>{l}</span> <input type="button" value="remove" onClick={() => this.removeFromField(i, 'languages')} /> </Fragment>))}

                        </SelectEditable>
                    </div>

                    <div>karma</div>
                    <div>throphies and badges</div>

                    <InputEditable field="tags" enterCallback={this.addTag} extraClasses="inline" tag="span" label="Your tags- that is, things your are interested about, or just want to enumerate." display={() => this.state.user.tags.length > 0 ?
                        this.state.user.tags.join(' ') : ''} changeUserField={() => this.setCurrentTag()} user={this.state.user} nodeBellow={this.state.user.tags.map((l, i) => (<Fragment key={i}> <span>{l}</span> <input type="button" value="remove" onClick={() => this.removeFromField(i, 'tags')} /> </Fragment>))}>
                    </InputEditable>

                    <br />

                    <InputEditable field="description" tag="span" label="Your description- that is, more about you or whatever. Leave blank for a new random description." changeUserField={this.changeUserField} user={this.state.user}></InputEditable>

                    <br />
                    <div>
                        <input type="button" onClick={this.save} value="Save your profile" />
                    </div>
                </div>
            </div>);
    }

    addTag = () => {
        this.setState((state, _) => {
            let newTags = state.user.tags;

            newTags.push(state.currentTag);

            return { user: { ...state.user, tags: newTags }, currentTag: '' };
        });
    }

    removeFromField = (index, field: keyof User) => {
        this.setState((state, _) => {
            let removed = state.user[field] as any[];

            removed.splice(index, 1);

            return { user: { ... this.state.user, [field]: state.user[field] } };
        });
    }

    setBirthday = (dateMethod) =>
        (event: ChangeEvent<input_source>) => {
            let value = parseInt(event.target.value);

            if (!isNaN(value))
                this.setState((state, _) => {
                    let birthday = state.user.birthday || new Date(1000, 1, 1);

                    dateMethod.call(birthday, value);

                    return { user: { ...this.state.user, birthday: birthday } };
                });
        };

    getBirthday = (dateMethod) =>
        melan.age(this.state.user.birthday) > 0 ? `${dateMethod.call(this.state.user.birthday)}` : '';

    setCurrentTag = () =>
        (event: ChangeEvent<HTMLInputElement>) => {
            this.setState({ currentTag: event.target.value });
        }

    save = () => {
        let user = {
            ...this.state.user,
            tags: this.state.user.tags.join('\n'),
            languages: this.state.user.languages.join(','),
            birthday: melan.age(this.state.user.birthday) > 0 ? `${this.state.user.birthday.getFullYear()}-${this.state.user.birthday.getMonth() + 1}-${this.state.user.birthday.getDate()}` : ''
        };

        melan.post('/user/update/profile', user, randomized => {
            this.setState((state, _) => {
                let newUser = { ...state.user };

                for (let key of Object.keys(randomized))
                    newUser[key] = randomized[key];

                return { user: newUser };
            });

            alert('Saved');
        });
    }
}

class SettingsProfile extends React.Component<SettingsProfileProps, SettingsProfileState>{

    state = { visible: this.tabFromUrl() };

    componentDidMount() {
        window.addEventListener('popstate', this.tabFromUrl);
    }

    tabFromUrl() {
        return location.href.includes('/profile') ? Tab.Profile : Tab.Settings;
    }

    activate = (tab) => {
        this.setState({ visible: tab });

        history.pushState({}, '', tab == Tab.Profile ? '/settings/profile' : '/settings');
    }

    checkActive(tab) {
        return this.state.visible == tab ? 'active' : '';
    }

    render() {
        return (
            <>
                <div className="contact-list">
                    <a href="/im"> BACK TO CHATS</a>
                    <div className={this.checkActive(Tab.Profile)} onClick={() => this.activate(Tab.Profile)}>
                        Profile
                    </div>
                    <div className={this.checkActive(Tab.Settings)} onClick={() => this.activate(Tab.Settings)}>
                        Settings
                    </div>
                </div>
                <div>
                    <Profile user={this.props.user} visible={this.state.visible == Tab.Profile} />
                    <Settings user={this.props.user} visible={this.state.visible == Tab.Settings} />
                </div>
            </>);
    }
}

declare var initialUser;

ReactDOM.render(<SettingsProfile user={initialUser} />, document.querySelector('#im'));