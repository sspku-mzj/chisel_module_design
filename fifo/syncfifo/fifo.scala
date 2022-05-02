package utils
import chisel3._
import chisel3.util._
import chisel3.experimental._

class fifo(val width : Int, val depth : Int) extends Module with RequireAsyncReset{
    val io = IO(new Bundle{
        val data_in = Input(UInt(width.W))
        val data_out = Output(UInt(width.W))

        val push = Input(Bool())
        val pop = Input(Bool())
        val flush = Input(Bool())

        val empty = Output(Bool())
        val not_empty = Output(Bool())
        val full = Output(Bool())
        val not_full = Output(Bool())
    })

    /al dataReg = RegInit(0.U(width.W))
    val wptr = RegInit(0.U(depth.W))
    val rptr = RegInit(0.U(depth.W))
    val mem = Mem(depth,UInt(width.W))
    val count = RegInit(0.U(depth.W))
    
    def Count(count : UInt):UInt = {
        Mux(count === (depth - 1).U,0.U,count + 1.U)
    }

    when(io.flush){
        wptr := 0.U
        rptr := 0.U
        count := 0.U
        io.data_out := 0.U
    }.otherwise{
        when(io.push === true.B && io.pop === false.B){
            io.data_out := 0.U
            when(count < depth.U){
                mem(wptr):=io.data_in
                wptr := Count(wptr)
                count := count + 1.U
            }
        }.elsewhen(io.push === false.B && io.pop === true.B){
            when(count > 0.U){
                io.data_out := mem(rptr)
                rptr := Count(rptr)
                count := count - 1.U
            }.otherwise{
                io.data_out := 0.U
            }
        }.elsewhen(io.push === true.B && io.pop === true.B){
            when(count === 0.U){
                io.data_out := io.data_in
            }.otherwise{
                io.data_out := mem(rptr)
                rptr := Count(rptr)
                mem(wptr) := io.data_in
                wptr := Count(wptr)
            }
        }.otherwise{
            io.data_out := 0.U
        }
    }
    io.full := (depth.U === count)
    io.empty := (count === 0.U)
    io.not_full := !io.full
    io.not_empty := !io.empty         
}
