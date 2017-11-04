import PropTypes from 'prop-types';
import React from 'react';
import ReactDOM from 'react-dom';
import fp from 'lodash/fp';
import { browserHistory } from 'react-router';
import { connect } from 'react-redux';

import { fetchUsers } from '../actions/actionTypes';
import ToolBarView from '../Components/ToolBarView';
import UserListView from '../Components/UserList';

class UserList extends React.Component {
  componentDidMount() {
  }

  render() {
    const users = this.props.users;

    return (
      <div>
        <ToolBarView title="Accounts" />
        <UserListView users={users} />
      </div>
    );
  }
};

UserList.propTypes = {
  apiConfig: PropTypes.object.isRequired,
  fetchUsers: PropTypes.func.isRequired,
  users: PropTypes.object.isRequired,
}

const mapDispatchToProps = (dispatch) => {
  return {
    fetchUsers: fetchUsers(dispatch)
  }
}

const mapStateToProps = (state) => {
  return {
    users: state.users,
    apiConfig: state.apiConfig
  };
}

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(UserList);

