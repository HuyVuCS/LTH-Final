// URL của server Haskell
const WS_URL = 'ws://localhost:9160';
const RECONNECT_DELAY = 3000; // Thời gian chờ trước khi kết nối lại (3 giây)

let socket = null; // Biến giữ kết nối WebSocket
let reconnectTimer = null; // Timer để kết nối lại

// Biến giữ callback, SẼ LUÔN ĐƯỢC CẬP NHẬT
let savedCallbacks = {
  onMessagesReceived: null,
  onConnectionError: null,
  onConnectionOpen: null
};

// --- Các hàm tiện ích ---

let messageCounter = 0; // Biến đếm để tạo ID duy nhất
const generateUniqueId = (timestamp) => {
  messageCounter = (messageCounter + 1) % 10000;
  return `${timestamp}_${messageCounter.toString().padStart(4, '0')}`;
};

export const getPrivateRoomId = (userId1, userId2) => {
  return [userId1, userId2].sort().join('_');
};

/**
 * Kết nối tới server WebSocket.
 * @param {function} onMessagesReceived - Callback để xử lý tin nhắn (BroadcastMessage)
 * @param {function} onConnectionError - Callback khi không thể kết nối
 * @param {function} onConnectionOpen - Callback khi kết nối thành công
 */
/**
 * Hàm nội bộ: Gán các trình xử lý sự kiện cho socket.
 * Nó sẽ LUÔN LUÔN gọi các hàm trong `savedCallbacks`.
 */
const setSocketListeners = () => {
  if (!socket) return;

  // 1. Xử lý khi kết nối MỞ thành công
  socket.onopen = () => {
    console.log('[ChatService] Đã kết nối WebSocket tới Haskell Server.');
    savedCallbacks.onConnectionOpen?.(); // Luôn gọi hàm đã lưu
  };

  // 2. Xử lý khi NHẬN được tin nhắn từ server
  socket.onmessage = (event) => {
    try {
      const wsMessage = JSON.parse(event.data);
      
      if (wsMessage.tag === "BroadcastMessage") {
        const message = wsMessage.contents;
        const timestamp = new Date(message.timestamp);
        const messageWithTimestamp = {
          ...message,
          id: generateUniqueId(timestamp.getTime()),
          timestamp: timestamp,
          receiverId: message.receiverId || null // Đảm bảo nó là null nếu không tồn tại
        };
        
        savedCallbacks.onMessagesReceived?.(messageWithTimestamp); // Luôn gọi hàm đã lưu
      }
    } catch (error) {
      console.error("[ChatService] Lỗi khi parse tin nhắn từ server:", error);
    }
  };

  // 3. Xử lý khi kết nối ĐÓNG
  socket.onclose = (event) => {
    console.log(`[ChatService] WebSocket đóng với mã: ${event.code}, lý do: ${event.reason || 'không có lý do'}`);
    
    // Kiểm tra xem socket vừa đóng (event.target) có phải là socket
    // đang hoạt động (biến `socket` toàn cục) hay không.
    const isCurrentSocket = (socket === event.target);

    if (event.code !== 1000) { // Đóng không chủ đích
      savedCallbacks.onConnectionError?.(); // Luôn gọi hàm đã lưu
      if (isCurrentSocket) {
        socket = null; // Chỉ gán null nếu đây là socket đang hoạt động
        scheduleReconnect();
      }
    } else {
      console.log('[ChatService] WebSocket đóng có chủ đích, không kết nối lại.');
      if (isCurrentSocket) {
        socket = null;
      }
    }
  };

  // 4. Xử lý LỖI
  socket.onerror = (error) => {
    console.error('[ChatService] Lỗi WebSocket:', error);
    savedCallbacks.onConnectionError?.(); // Luôn gọi hàm đã lưu
  };
};

/**
 * Kết nối tới server WebSocket.
 */
export const connect = (onMessagesReceived, onConnectionError, onConnectionOpen) => {
  
  // BƯỚC 1: Luôn cập nhật các callback mới nhất từ component React
  savedCallbacks = {
    onMessagesReceived,
    onConnectionError,
    onConnectionOpen
  };

  if (reconnectTimer) {
    clearTimeout(reconnectTimer);
    reconnectTimer = null;
  }

  // BƯỚC 2: Nếu socket đã tồn tại và đang kết nối/mở, không làm gì cả.
  if (socket && (socket.readyState === WebSocket.OPEN || socket.readyState === WebSocket.CONNECTING)) {
    console.log("[ChatService] WebSocket đã kết nối. Chỉ cập nhật callbacks.");
    return;
  }

  // Đóng kết nối cũ nếu có
  if (socket) {
    socket.close();
    socket = null;
  }

  // BƯỚC 3: Tạo kết nối mới
  console.log(`[ChatService] Đang tạo kết nối WebSocket mới tới ${WS_URL}...`);
  try {
    socket = new WebSocket(WS_URL);
    
    // Gán listeners CHỈ MỘT LẦN KHI TẠO MỚI
    setSocketListeners(); 
    
  } catch (error) {
    console.error("[ChatService] Lỗi tạo WebSocket:", error);
    scheduleReconnect();
    savedCallbacks.onConnectionError?.();
  }
};

/**
 * Gửi tin nhắn mới tới server Haskell
 */
// HÀM HELPER NỘI BỘ (thay cho logic trong sendMessage cũ)
const sendWsMessage = (wsMessage) => {
  if (!socket || socket.readyState !== WebSocket.OPEN) {
    console.error("[ChatService] Không thể gửi: WebSocket không mở.");
    return false;
  }
  try {
    socket.send(JSON.stringify(wsMessage));
    return true;
  } catch (error) {
    console.error("[ChatService] Lỗi gửi tin nhắn:", error);
    return false;
  }
};

// HÀM ĐỊNH DANH
// (Báo cho server biết "Tôi là user1")
export const identifyUser = (userId) => {
  console.log(`[ChatService] Định danh là: ${userId}`);
  return sendWsMessage({
    tag: "Identify",
    contents: userId // Nội dung là "user1", "user2"...
  });
};

// HÀM GỬI TIN NHÓM
// (Thay thế cho hàm sendMessage cũ)
export const sendGroupMessage = (messageData) => {
  return sendWsMessage({
    tag: "SendChatMessage",
    contents: messageData
  });
};

// HÀM GỬI TIN RIÊNG
export const sendPrivateMessage = (toUserId, messageData) => {
  console.log(`[ChatService] Gửi tin riêng cho: ${toUserId}`);
  return sendWsMessage({
    tag: "SendPrivateMessage",
    contents: [toUserId, messageData] // Khớp với [Text, Message] của Haskell
  });
};

/**
 * Đóng kết nối WebSocket
 */
/**
 * Lên lịch kết nối lại sau một khoảng thời gian
 */
const scheduleReconnect = () => {
  if (reconnectTimer) return; // Đã có timer đang chạy

  console.log(`[ChatService] Sẽ thử kết nối lại sau ${RECONNECT_DELAY}ms...`);
  
  reconnectTimer = setTimeout(() => {
    reconnectTimer = null;
    // Chỉ kết nối lại nếu socket thực sự không tồn tại hoặc đã ĐÓNG
    if (!socket || socket.readyState === WebSocket.CLOSED) {
      console.log('[ChatService] Đang thử kết nối lại...');
      
      // Gọi lại connect với các hàm đã lưu trong savedCallbacks
      connect(
        savedCallbacks.onMessagesReceived,
        savedCallbacks.onConnectionError,
        savedCallbacks.onConnectionOpen
      );
    }
  }, RECONNECT_DELAY);
};

/**
 * Đóng kết nối WebSocket và hủy việc tự động kết nối lại
 */
export const disconnect = () => {
  // Hủy timer reconnect nếu có
  if (reconnectTimer) {
    clearTimeout(reconnectTimer);
    reconnectTimer = null;
  }

  // Xóa các callback đã lưu
  savedCallbacks = {
    onMessagesReceived: null,
    onConnectionError: null,
    onConnectionOpen: null
  };

  // Đóng socket nếu đang mở
  if (socket) {
    socket.close(1000, "User initiated disconnect");
  }
};