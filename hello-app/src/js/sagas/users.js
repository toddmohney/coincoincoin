import 'whatwg-fetch';
import { takeEvery, put, call } from 'redux-saga/effects';
import {
  handleApiError
} from '../lib/apiErrorHandler';
import {
  FETCH_USERS,
  FETCH_USERS_SUCCESS,
  FETCH_USERS_FAILED,
  LOGOUT,
} from '../actions/actionTypes';

function fetchUsersApi(apiConfig) {
  return fetch(apiConfig.host + "/api/users", {
    method: 'GET'
  })
  .then(handleApiError)
  .then(function(response) {
    return response.json();
  })
  .catch(function(error){
    throw error;
  });
}

function* fetchUsers(action) {
  try {
    const response = yield call(
      fetchUsersApi,
      action.payload.apiConfig
    );

    yield put({
      type: FETCH_USERS_SUCCESS,
      payload: { records: response }
    });
  } catch (error) {
    yield put({
      type: FETCH_USERS_FAILED,
      error: error.message
    });
  }
}

export function* watchFetchUsers() {
  yield takeEvery(FETCH_USERS, fetchUsers);
}
