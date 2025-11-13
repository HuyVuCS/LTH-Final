import React from 'react';
import { useAuth, USERS } from '../context/AuthContext';
import './LoginPage.css';

const LoginPage = () => {
  const { login } = useAuth();
  const userList = Object.values(USERS);

  return (
    <div className="login-container">
      <div className="login-box">
        <h1 className="login-title">Haskell Messenger</h1>
        <p className="login-subtitle">Chọn profile để đăng nhập (Test real-time)</p>
        <div className="user-list">
          {userList.map(user => (
            <button
              key={user.id}
              onClick={() => login(user.id)}
              className="user-button"
            >
              <img src={user.img} alt={user.name} className="user-avatar" />
              <span className="user-name">{user.name}</span>
            </button>
          ))}
        </div>
        <p style={{marginTop: '20px', fontSize: '0.8rem', color: '#888'}}>
          (Mở 2 tab, chọn 2 user khác nhau để thử chat)
        </p>
      </div>
    </div>
  );
};

export default LoginPage;