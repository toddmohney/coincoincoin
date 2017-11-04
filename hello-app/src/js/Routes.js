import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import App from './containers/app';
import UserList from './containers/userList';

export default (
  <Route path="/" component={App}>
    <IndexRedirect to="/users" />
    <Route path="/users" component={UserList} />
  </Route>
);

