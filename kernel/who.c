/*
 *  linux/kernel/sys.c
 *
 *  (C) 2025  dr0ctopus1145
 */
/* kernel/who.c */

#include <errno.h>
#include <string.h>
#include <linux/sched.h> // for current, needed in real kernel code
#include <asm/segment.h> // for get_fs_byte, put_fs_byte, put_fs_long
#include <sys/types.h>

/* 最大名字长度限制，不包括终止符 '\0' */
#define NAME_MAX_LEN 23 

/* 存储用户设置的名字，需要多一个字节存放 '\0' */
static char saved_name[NAME_MAX_LEN + 1] = {0}; 
static int name_len = 0;

/*
 * iam() 系统调用实现
 * int iam(const char * name)
 */
int sys_iam(const char * name)
{
    char tmp_name[NAME_MAX_LEN + 1];
    int count = 0;
    char c;

    /* 1. 检查并复制用户空间字符串 */
    while ((c = get_fs_byte(name + count)) != '\0') {
        if (count >= NAME_MAX_LEN) {
            /* 长度超过 23 个字符 */
            printk("iam() error: name too long (> %d)\n", NAME_MAX_LEN);
            current->errno = EINVAL;
            return -1;
        }
        tmp_name[count] = c;
        count++;
    }

    /* 2. 拷贝到内核全局变量中 */
    if (count > 0) {
        memcpy(saved_name, tmp_name, count);
    }
    saved_name[count] = '\0';
    name_len = count;

    /* 3. 返回拷贝的字符数 */
    return name_len;
}

/*
 * whoami() 系统调用实现
 * int whoami(char* name, unsigned int size)
 */
int sys_whoami(char * name, unsigned int size)
{
    /* 1. 检查用户提供的缓冲区大小 */
    if (size < (name_len + 1)) {
        /* name_len + 1 是存储名字和终止符 '\0' 所需的最小空间 */
        printk("whoami() error: buffer size too small (need %d, got %d)\n", 
               name_len + 1, size);
        current->errno = EINVAL;
        return -1;
    }

    /* 2. 将内核中保存的名字拷贝到用户空间 */
    int i;
    for (i = 0; i <= name_len; i++) {
        /* i <= name_len 确保拷贝终止符 '\0' */
        put_fs_byte(saved_name[i], name + i);
    }

    /* 3. 返回拷贝的字符数 (不包括 '\0') */
    return name_len;
}
