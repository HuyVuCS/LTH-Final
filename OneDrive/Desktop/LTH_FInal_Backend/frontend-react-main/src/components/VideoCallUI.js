import React from 'react';
import './VideoCallUI.css'; // Chúng ta sẽ tạo file CSS này

const VideoCallUI = ({ status, participants, onEndCall }) => {
  
  return (
    <div className="videocall-overlay">
      <div className="videocall-modal">
        <h2 className="videocall-title">
          Cuộc gọi Nhóm (3 thành viên)
        </h2>
        <p className="videocall-status">
          Trạng thái: {status === 'CONNECTING' ? 'Đang kết nối...' : 'Đang trong cuộc gọi'}
        </p>

        <div className="videocall-grid">
          {/* Video của chính mình */}
          <div className="video-participant self-video">
            Bạn (Local)
          </div>

          {/* Video các thành viên khác */}
          {participants.map((p) => (
            <div key={p.id} className="video-participant remote-video">
              {p.name} (Remote)
            </div>
          ))}

          {/* Hiển thị các ô trống nếu chưa đủ 3 người (ví dụ) */}
          {participants.length < 2 && (
             <div className="video-participant empty-slot">
              Đang chờ...
            </div>
          )}
        </div>

        {/* Thanh điều khiển */}
        <div className="videocall-controls">
          <button
            onClick={onEndCall}
            className="control-button end-call"
          >
            Kết thúc
          </button>
          {/* Các nút Mute/Video Off khác có thể thêm vào đây */}
        </div>
      </div>
    </div>
  );
};

export default VideoCallUI;