import React, { useState, useEffect, useRef } from 'react';
import { useAuth } from '../context/AuthContext';
import { 
  connect, 
  disconnect, 
  identifyUser,         
  sendGroupMessage,     
  sendPrivateMessage
} from '../services/chatService';
import { CALL_STATUS } from '../services/stringeeService';
import VideoCallUI from '../components/VideoCallUI';
import './ChatRoom.css'; 

// Tạo một đối tượng giả cho kênh chung
const GROUP_CHAT = {
  id: 'group_chat',
  name: '# Kênh chung Haskell',
  img: 'https://placehold.co/50x50/8B5CF6/FFFFFF?text=G'
};

const ChatRoom = () => {
  const { currentUser, logout, otherUsers, USERS } = useAuth();
  
  // State mới: Bạn đang chat với ai? (Mặc định là kênh chung)
  const [currentChatTarget, setCurrentChatTarget] = useState(GROUP_CHAT);
  
  const [messages, setMessages] = useState([]); // TẤT CẢ tin nhắn
  const [newMessage, setNewMessage] = useState('');
  const [isWsConnected, setIsWsConnected] = useState(false);
  
  const [callState, setCallState] = useState({
    status: CALL_STATUS.IDLE,
    participants: [],
  });
  
  const messagesEndRef = useRef(null);

  // Lắng nghe tin nhắn real-time
  useEffect(() => {
    // 1. Hàm callback khi nhận tin nhắn (CẢ CHUNG VÀ RIÊNG)
    const handleNewMessage = (receivedMessage) => {
      // Chỉ cần thêm vào state tổng. Việc lọc sẽ do UI đảm nhiệm.
      setMessages(prev => [...prev, receivedMessage]);
    };
    
    // 2. Hàm callback khi lỗi/mất kết nối
    const handleError = () => {
      setIsWsConnected(false);
      setMessages(prev => [...prev, {
        id: `err_${Date.now()}`,
        senderId: 'user2',
        senderName: 'Bot (Hệ thống)',
        content: 'LỖI: Mất kết nối tới Haskell WebSocket Server.',
        timestamp: new Date(),
        receiverId: null // Tin hệ thống là tin chung
      }]);
    };
    
    // 3. Hàm callback khi kết nối thành công
    const handleOpen = () => {
        setIsWsConnected(true);
        setMessages(prev => [...prev, {
            id: `ok_${Date.now()}`,
            senderId: 'user2',
            senderName: 'Bot (Hệ thống)',
            content: 'Đã kết nối thành công tới Haskell Server!',
            timestamp: new Date(),
            receiverId: null // Tin hệ thống là tin chung
        }]);
        // ĐỊNH DANH USER VỚI SERVER
        identifyUser(currentUser.id);
    };

    // 4. Kết nối!
    connect(handleNewMessage, handleError, handleOpen);

    // 5. Cleanup: Ngắt kết nối khi component unmount
    return () => {
      disconnect();
    };
  }, [currentUser.id]); // Thêm currentUser.id làm dependency

  // Cuộn xuống tin nhắn mới nhất
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages, currentChatTarget]); // Chạy khi đổi phòng hoặc có tin nhắn mới


  // Xử lý gửi tin nhắn (ĐÃ CẬP NHẬT)
  const handleSendMessage = (e) => {
    e.preventDefault();
    if (newMessage.trim() === '' || !isWsConnected) return;

    // messageData không cần receiverId, backend sẽ xử lý
    const messageData = {
      senderId: currentUser.id,
      senderName: currentUser.name, 
      content: newMessage.trim(),
      timestamp: new Date().toISOString()
    };
    
    let sent = false;
    if (currentChatTarget.id === 'group_chat') {
      // 1. Gửi tin chat CHUNG
      sent = sendGroupMessage(messageData);
    } else {
      // 2. Gửi tin chat RIÊNG
      sent = sendPrivateMessage(currentChatTarget.id, messageData);
    }
    
    if (!sent) {
      setMessages(prev => [...prev, {
        id: `err_send_${Date.now()}`,
        senderId: 'user2',
        senderName: 'Bot (Hệ thống)',
        content: 'Không thể gửi tin nhắn. Đang thử kết nối lại...',
        timestamp: new Date(),
        receiverId: null
      }]);
      return;
    }
    
    // Chờ server broadcast về
    setNewMessage('');
  };

  // --- LỌC TIN NHẮN ĐỂ HIỂN THỊ ---
  const displayedMessages = messages.filter(msg => {
    const targetId = currentChatTarget.id;
    const myId = currentUser.id;
    
    if (targetId === 'group_chat') {
      // Tin nhắn chung là tin không có receiverId
      return !msg.receiverId;
    }
    
    // Lọc tin nhắn riêng:
    // (Gửi từ TÔI đến NGƯỜI ẤY) HOẶC (Gửi từ NGƯỜI ẤY đến TÔI)
    return (msg.senderId === myId && msg.receiverId === targetId) ||
           (msg.senderId === targetId && msg.receiverId === myId);
  });
  
  // Tên phòng chat
  const getRoomName = () => currentChatTarget.name;

  // Xử lý cuộc gọi (không đổi)
  const startGroupCall = async () => { /* ... */ };
  const endCall = () => { /* ... */ };


  return (
    <div className="chatroom-container">
      {/* 1. Thanh bên (Sidebar) */}
      <div className="sidebar">
        <div className="sidebar-header">
          <img src={currentUser.img} alt={currentUser.name} className="sidebar-avatar" />
          <div className="sidebar-user-info">
            <span className="sidebar-username">{currentUser.name}</span>
            <span className={`sidebar-status ${isWsConnected ? 'connected' : 'disconnected'}`}>
              {isWsConnected ? 'Đã kết nối' : 'Mất kết nối'}
            </span>
          </div>
        </div>
        
        <div className="sidebar-menu">
          <h4 className="sidebar-title">Kênh Chat</h4>
          {/* Nút Kênh Chung */}
          <div 
            className={`channel-item ${currentChatTarget.id === 'group_chat' ? 'active' : ''}`}
            onClick={() => setCurrentChatTarget(GROUP_CHAT)}
          >
            {GROUP_CHAT.name}
          </div>
        </div>
        
        <div className="sidebar-users">
           <h4 className="sidebar-title">Chat Riêng</h4>
           {/* Danh sách User (Đã Bật) */}
           {otherUsers.map(user => (
               <div 
                 key={user.id} 
                 className={`user-item ${currentChatTarget.id === user.id ? 'active' : ''}`}
                 onClick={() => setCurrentChatTarget(user)}
                >
                 <img src={user.img} alt={user.name} className="user-item-avatar" />
                 <span>{user.name}</span>
                 {/* (Có thể thêm chỉ báo tin nhắn mới ở đây) */}
               </div>
           ))}
        </div>

        <button onClick={logout} className="logout-button">
          Đăng xuất (Đổi Profile)
        </button>
      </div>

      {/* 2. Khung chat chính */}
      <div className="chat-area">
        <div className="chat-header">
          {/* Tên phòng chat động */}
          <h3>{getRoomName()}</h3>
            <button 
              onClick={startGroupCall} 
              className="call-button"
              disabled={callState.status !== CALL_STATUS.IDLE}
            >
              {/* (Icon) */}
              Gọi Video Nhóm
            </button>
        </div>

        <div className="message-list">
          {/* Render tin nhắn đã lọc */}
          {displayedMessages.map((msg) => (
            <div
              key={msg.id}
              className={`message-item ${msg.senderId === currentUser.id ? 'sent' : 'received'}`}
            >
              <img 
                src={USERS[msg.senderId]?.img} 
                alt={msg.senderName} 
                className="message-avatar" 
                title={msg.senderName}
              />
              <div className="message-content">
                <div className="message-sender">
                  {msg.senderName} 
                  <span className="message-timestamp">
                    {new Date(msg.timestamp).toLocaleTimeString()}
                  </span>
                </div>
                <div className="message-bubble">
                  <p>{msg.content}</p>
                </div>
              </div>
            </div>
          ))}
          <div ref={messagesEndRef} />
        </div>

        <form onSubmit={handleSendMessage} className="message-input-form">
          <input
            type="text"
            value={newMessage}
            onChange={(e) => setNewMessage(e.target.value)}
            placeholder={isWsConnected ? `Nhắn tin vào ${getRoomName()}` : 'Đang kết nối...'}
            className="message-input"
            disabled={!isWsConnected}
            autoComplete="off"
          />
          <button 
            type="submit" 
            className="send-button" 
            disabled={!newMessage.trim() || !isWsConnected}
          >
            Gửi
          </button>
        </form>
      </div>

      {/* 3. Giao diện Video Call (Không đổi) */}
      {callState.status !== CALL_STATUS.IDLE && (
        <VideoCallUI
          status={callState.status}
          participants={callState.participants}
          onEndCall={endCall}
        />
      )}
    </div>
  );
};

export default ChatRoom;