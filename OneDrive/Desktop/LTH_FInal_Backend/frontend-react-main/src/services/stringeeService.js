export const CALL_STATUS = {
  IDLE: 'IDLE',
  CONNECTING: 'CONNECTING',
  IN_CALL: 'IN_CALL',
};

const stringeeService = {
 
  joinGroupCall: async (currentUserId) => {
    console.log(`[StringeeService] ${currentUserId} yêu cầu tham gia Group Call...`);
    // Trong thực tế: const groupCall = new Stringee.Room('room_name'); groupCall.join();
    // Ở đây, chúng ta gọi API backend Haskell để lấy quyền tham gia
    await new Promise(resolve => setTimeout(resolve, 1500)); // Giả lập API call
    
    console.log("[StringeeService] Đã tham gia Group Call thành công (Giả lập).");
    return {
      status: 'success',
      roomName: 'haskell_chat_room_123',
      participants: [
        { id: 'user2', name: 'Bob' },
        { id: 'user3', name: 'Charlie' },
      ].filter(p => p.id !== currentUserId) 
    };
  },

  endCall: () => {
    console.log('[StringeeService] Kết thúc cuộc gọi (Giả lập)...');
    return true;
  }
};

export default stringeeService;