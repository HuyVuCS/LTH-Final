const API_URL = 'http://localhost:9160';

/**
 * Xử lý kết quả fetch, parse JSON và bắt lỗi
 */
const handleResponse = async (response) => {
  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Lỗi API: ${response.status} ${response.statusText} - ${errorText}`);
  }
  return response.json();
};

/**
 * Định dạng tin nhắn từ CSDL
 */
const formatMessage = (msg) => ({
  ...msg,
  // Đảm bảo ID là duy nhất (dùng tạm timestamp nếu id không có)
  id: msg.id || `${msg.senderId}_${msg.timestamp}`, 
  timestamp: new Date(msg.timestamp), // Chuyển string thành Date object
  receiverId: msg.receiverId || null, // Đảm bảo receiverId là null
});

/**
 * Lấy TOÀN BỘ lịch sử chat (cả nhóm và riêng tư)
 * (Gọi GET /messages)
 */
export const fetchAllMessages = async () => {
  try {
    const response = await fetch(`${API_URL}/messages`);
    const messages = await handleResponse(response);
    return messages.map(formatMessage);
  } catch (error) {
    console.error("Lỗi khi tải toàn bộ lịch sử chat:", error);
    return []; // Trả về mảng rỗng nếu lỗi
  }
};