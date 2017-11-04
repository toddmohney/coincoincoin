import PropTypes from 'prop-types';
import React from 'react';
import ReactDOM from 'react-dom';
import { Toolbar, ToolbarGroup, ToolbarTitle } from 'material-ui/Toolbar';

class ToolBarView extends React.Component {
  render() {
    const { title } = this.props;

    return (
      <Toolbar>
        <ToolbarGroup>
          <ToolbarTitle text={title} />
        </ToolbarGroup>
      </Toolbar>
    );
  }
};

ToolBarView.propTypes = {
  title: PropTypes.string.isRequired,
}

export default ToolBarView;
