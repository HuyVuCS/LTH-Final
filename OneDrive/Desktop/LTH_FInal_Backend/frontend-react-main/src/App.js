import React from 'react';
import { useAuth } from './context/AuthContext';
import LoginPage from './pages/LoginPage';
import ChatRoom from './pages/ChatRoom';
import './App.css'; // Import file CSS chính

function App() {
  const { currentUser, isAuthReady } = useAuth();

  if (!isAuthReady) {
    return (
      <div className="loading-container">
        <div className="loading-spinner"></div>
        <p>Đang khởi tạo...</p>
      </div>
    );
  }

  if (!currentUser) {
    return <LoginPage />;
  }

  return (
    <div className="App">
      <ChatRoom />
    </div>
  );
}

export default App;