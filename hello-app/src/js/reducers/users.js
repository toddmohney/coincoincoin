import {
  FETCH_USERS,
  FETCH_USERS_SUCCESS,
  FETCH_USERS_FAILED,
} from '../actions/actionTypes';

const initialState = {
  records: [],
  status: {
    isStale: true,
    isFetching: false,
    fetchError: null
  }
};

const reducer = function usersReducer (state = initialState, action) {
  let newState;

  switch (action.type) {
    case FETCH_USERS:
      return Object.assign({}, state, {
        status: {
          isFetching: true,
          fetchError: null
        }
      });

    case FETCH_USERS_SUCCESS:
      return Object.assign({}, state, {
        records: action.payload.records,
        status: {
          isStale: false,
          isFetching: false,
          fetchError: null
        }
      });

    case FETCH_USERS_FAILED:
      return Object.assign({}, state, {
        status: {
          isStale: true,
          isFetching: false,
          fetchError: action.error
        }
      });

    default:
      return state;
  }
};

export default reducer;
