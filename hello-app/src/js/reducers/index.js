import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';

import apiConfig from './apiConfig';
import users from './users';

const rootReducer = combineReducers({
  apiConfig,
  routing: routerReducer,
  users,
});

export default rootReducer;
