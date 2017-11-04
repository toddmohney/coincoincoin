pragma solidity ^0.4.18;

contract Hellos {
  mapping (address => uint) public hellos;

  function sayHello() public {
    hellos[msg.sender] += 1;
  }

  function getHellos() public view returns (uint) {
      return hellos[msg.sender];
  }
}
