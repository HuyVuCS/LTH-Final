import React, { createContext, useState, useContext } from 'react';

// 3 người dùng bình thường
export const USERS = {
  user1: { id: 'user1', name: 'Alice', img: 'https://placehold.co/50x50/34D399/FFFFFF?text=A', stringeeUserId: 'alice_stringee' },
  user2: { id: 'user2', name: 'Bob', img: 'https://placehold.co/50x50/60A5FA/FFFFFF?text=B', stringeeUserId: 'bob_stringee' },
  user3: { id: 'user3', name: 'Charlie', img: 'https://placehold.co/50x50/FCD34D/000000?text=C', stringeeUserId: 'charlie_stringee' },
};

const AuthContext = createContext(null);

export const AuthProvider = ({ children }) => {
  const [selectedProfile, setSelectedProfile] = useState(null); // Profile (Alice, Bob, Charlie)
  // eslint-disable-next-line no-unused-vars
  const [isAuthReady, setIsAuthReady] = useState(true); // Giữ lại để xử lý trạng thái auth trong tương lai

  const login = (userId) => {
    const user = USERS[userId];
    if (user) {
      setSelectedProfile(user);
    }
  };

  const logout = () => {
    setSelectedProfile(null);
  };
  
  const otherUsers = Object.values(USERS).filter(
    u => u.id !== selectedProfile?.id
  );

  return (
    <AuthContext.Provider value={{ 
      currentUser: selectedProfile, 
      isAuthReady, // Giữ lại cho App.js
      login, 
      logout, 
      USERS,
      otherUsers 
    }}>
      {children}
    </AuthContext.Provider>
  );
};

export const useAuth = () => {
  return useContext(AuthContext);
};